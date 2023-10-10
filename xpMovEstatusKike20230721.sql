SET DATEFIRST 7
SET ANSI_NULLS OFF
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
SET LOCK_TIMEOUT -1
SET QUOTED_IDENTIFIER OFF
GO

IF EXISTS(SELECT * FROM sysobjects WHERE TYPE='' AND NAME='')
DROP PROCEDURE 
GO
CREATE PROCEDURE xpMovEstatus
		@Empresa		char(5),
		@Sucursal		int,
		@Modulo			char(5),
		@ID			int,
		@Estatus		char(15),
		@EstatusNuevo		char(15),
		@Usuario		char(10),
		@FechaEmision		datetime,
		@FechaRegistro		datetime,
		@Mov			char(20),
		@MovID			varchar(20),
		@MovTipo		char(20),
		@Ok			int		OUTPUT,
		@OkRef			varchar(255)	OUTPUT

AS BEGIN
  DECLARE	@Almacen		char(10),
		@MaxFecha		datetime,
		@IDCxp			int,
		@IDCxp2			int,
		@IDCxp3			int,
		@MovCxp			char(20),
		@MovIDCxp		char(20),
		@MovIDCxp2		char(20),
		@MovIDAplicaCxp		char(20),
		@Importe		money,
		@Impuestos		money,
		@Proveedor		char(10),
		@Bonificacion		float,
		@BonificacionTipo	char(10),
		@OrigenTipo       varchar(20),
		@Origen			char(20),
		@OrigenID         char(20),
		@MovTipoOrigen		char(10),
		@CompraID         int,
		@Articulo         varchar(20),
		@Cantidad         float,
		@IDAplica           int,
		@EstatusActualizar  varchar(15),
		@InvID              int,
		@InvMov             varchar(20),
		@VentaID            int,
    @Asunto             varchar(255)

  IF @Modulo = 'EMB' AND @MovTipo = 'EMB.E'
  BEGIN
    EXEC spDicoEmbarque @Empresa, @Sucursal, @Modulo, @ID, @Estatus, @EstatusNuevo, @Usuario, @FechaEmision, @FechaRegistro,
                        @Mov, @MovID, @MovTipo, @Ok OUTPUT, @OkRef OUTPUT

  END

  IF @Modulo = 'VTAS' AND @MovTipo = 'VTAS.P' AND @EstatusNuevo IN ('PENDIENTE', 'CANCELADO')
  BEGIN
    SELECT @Articulo = MIN(Cuenta)
      FROM AuxiliarU
     WHERE ModuloID = @ID
       AND Modulo = 'VTAS'
       AND Rama = 'RESV'
     GROUP BY Cuenta, Grupo
    HAVING SUM(CargoU) < SUM(AbonoU)

    IF @Articulo IS NOT NULL
    BEGIN
      SELECT @Ok = 10060, @OkRef = 'El Articulo ' + @Articulo + ' tuvo mas Abonos que Cargos' 

      SELECT @Asunto = 'Error ' + @Mov + ' ' + @MovID + ' Artículo ' + @Articulo

      EXECUTE msdb.dbo.sp_send_dbmail
            @profile_name = 'Perfil',
            @recipients   = 'etoral@intelisis.com; micorreo@gmail.com',
            @subject      = @Asunto,
            @body         = @Asunto
            
    END
  END

-- 29/May/2009 Enrique Toral. Se modifica cálculo y validación de presupuesto
--  IF @Modulo in ('COMS', 'VTAS') AND (@MovTipo IN ('COMS.PR', 'COMS.F', 'COMS.EG', 'COMS.EI', 'COMS.D', 'COMS.O', 'COMS.CP',  'VTAS.PR', 'VTAS.D', 'VTAS.F', 'VTAS.VP', 'VTAS.FM') OR (@MovTipo = 'VTAS.P' AND @Mov in ('Cambio Modelo', 'Pedido', 'Pedido Venta Piso', 'Reservado')))
  IF @Modulo in ('VTAS') AND (@MovTipo IN ('VTAS.PR', 'VTAS.D', 'VTAS.F', 'VTAS.VP', 'VTAS.FM') OR (@MovTipo = 'VTAS.P' AND @Mov in ('Cambio Modelo', 'Pedido', 'Pedido Venta Piso', 'Reservado')))
     AND ((@Estatus = 'SINAFECTAR' AND @EstatusNuevo IN ('CONCLUIDO', 'PENDIENTE', 'PROCESAR')) OR (@Estatus in ('CONCLUIDO', 'PENDIENTE', 'PROCESAR') AND @EstatusNuevo = 'CANCELADO')) AND @Ok IS NULL
  BEGIN
    EXEC spDicoPresupuestoCat @Empresa, @Sucursal, @Modulo, @ID, @Estatus, @EstatusNuevo, @Usuario, @FechaEmision, @FechaRegistro,
                          @Mov, @MovID, @MovTipo, @Ok OUTPUT, @OkRef OUTPUT
  END

  -- Genera Cargo/Credito Proveedor en lugar de Descuento al costo
  IF @Modulo = 'COMS' AND @MovTipo IN ('COMS.F', 'COMS.EG', 'COMS.EI', 'COMS.D') AND (@Estatus = 'SINAFECTAR' AND @EstatusNuevo = 'CONCLUIDO')
  BEGIN
    SELECT @Bonificacion = ISNULL(AutoCargos, 0), @Proveedor = Proveedor FROM Compra WHERE ID = @ID
    SELECT @BonificacionTipo = ISNULL(CompraAutoCargosTipo, 'No') FROM Prov WHERE Proveedor = @Proveedor
    SELECT @IDCxp = NULL, @IDCxp2 = NULL, @IDCxp3 = NULL
    IF @BonificacionTipo in ('', 'No') AND ISNULL(@Bonificacion, 0) > 0
    BEGIN
      SELECT @MovCxp = CASE @MovTipo WHEN 'COMS.D' THEN CxpCargoProveedor ELSE CxpCreditoProveedor END,
             @MovIDAplicaCxp = CxpAplicacion
        FROM EmpresaCfgMov
       WHERE Empresa = @Empresa

      SELECT @Importe = SUM(Cantidad*Costo), @Impuestos = SUM((Cantidad*Costo)*(Impuesto1/100))
        FROM CompraD d
       WHERE d.ID = @ID

      SELECT @Importe = @Importe*(@Bonificacion/100), @Impuestos = ISNULL(@Impuestos, 0)*(@Bonificacion/100)

      INSERT INTO Cxp(Empresa, Mov,     FechaEmision, Proyecto, UEN, Moneda, TipoCambio, Usuario, Estatus, Proveedor, ProveedorMoneda, ProveedorTipoCambio, Importe, Impuestos, AplicaManual, Sucursal, SucursalOrigen, OrigenTipo, Origen, OrigenID)
               SELECT Empresa, @MovCxp, FechaEmision, Proyecto, UEN, Moneda, TipoCambio, Usuario, 'SINAFECTAR', Proveedor, Moneda, TipoCambio, @Importe, @Impuestos, 1, Sucursal, SucursalOrigen, 'BONIF', Mov, MovID
                 FROM Compra
                WHERE ID = @ID

      SELECT @IDCxp = @@IDENTITY

      IF @MovTipo <> 'COMS.D' AND @IDCxp is not null
      BEGIN
        INSERT INTO CxpD(ID, Renglon, RenglonSub, Importe, Aplica, AplicaID, Sucursal, SucursalOrigen)
                 VALUES(@IDCxp, 2048, 0, @Importe+@Impuestos, @Mov, @MovID, @Sucursal, @sucursal)
      END

      IF @IDCxp is not null
        EXEC spAfectar 'CXP', @IDCxp, 'AFECTAR', @EnSilencio = 1, @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT, @Conexion = 1

      IF @Ok is null AND @IDCxp is not null
      BEGIN
        SELECT @MovIDCxp = MovID FROM Cxp WHERE ID = @IDCxp
        EXEC spMovFlujo @Sucursal, 'AFECTAR', @Empresa, 'COMS', @ID, @Mov, @MovID, 'CXP', @IDCxp, @MovCxp, @MovIDCxp, @Ok = @Ok OUTPUT
      END

      IF @MovTipo = 'COMS.D' 
      BEGIN
        SELECT @IDCxp2 = ID 
          FROM Cxp
         WHERE Empresa = @Empresa
           AND Estatus = 'PENDIENTE'
           AND Proveedor = @Proveedor
           AND Origen = @Mov AND OrigenID = @MovID

        SELECT @MovIDCxp2 = DMovID
          FROM MovFlujo WHERE OModulo = 'COMS' 
           AND OID = @ID 
           AND DModulo = 'CXP'
           AND DMov = @MovCxp

        IF @IDCxp2 is not null
        BEGIN
          EXEC spCx @IDCxp2, 'CXP', 'GENERAR', 'Todo', @FechaRegistro, @MovIDAplicaCxp, 
                    @Usuario, 1, 0, NULL, NULL, @IDGenerar = @IDCxp3 OUTPUT, @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT

--          UPDATE Cxp SET Concepto = 'ANTICIPO PROVEEDORES' WHERE ID = @IDCxp3
          UPDATE Cxp SET Concepto = 'DEVOLUCION DE MERCANCIA' WHERE ID = @IDCxp3

          UPDATE Cxp SET Importe = @Importe + ISNULL(@Impuestos, 0) WHERE ID = @IDCxp3

          IF @IDCxp3 IS NOT NULL
            INSERT INTO CxpD(ID, Renglon, RenglonSub, Importe, Aplica, AplicaID, Sucursal, SucursalOrigen)
                   VALUES(@IDCxp3, 2048, 0, @Importe+@Impuestos, @MovCxp, @MovIDCxp2, @Sucursal, @Sucursal)

          EXEC spAfectar 'CXP', @IDCxp3, 'AFECTAR', @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT, @Conexion = 1, @EnSilencio = 1

        END
      END -- COMS.D
    END -- Bonficiacion > 0
  END  -- End Genera Cargo/Credito 

  IF @Modulo = 'COMS' AND @MovTipo = 'COMS.O' AND @Estatus = 'SINAFECTAR' AND @EstatusNuevo = 'PENDIENTE'
  BEGIN
    SELECT @MaxFecha = Max(FechaEntrega) FROM CompraD WHERE ID = @ID
    IF @MaxFecha IS NOT NULL
      UPDATE Compra SET FechaEntrega = @MaxFecha WHERE ID = @ID
  END
  
  IF @Modulo = 'COMS' AND @MovTipo IN ('COMS.F', 'COMS.EG', 'COMS.EI') AND @Ok IS NULL AND (@Estatus = 'SINAFECTAR' AND @EstatusNuevo = 'CONCLUIDO')
  BEGIN
  	-- Reservar Ventas asignadas a Orden de Compra
  	DECLARE crCompraDVenta CURSOR FOR
  	SELECT c.ID, cd.Articulo, SUM(cd.Cantidad)
  	  FROM CompraD cd
      JOIN MovTipo m ON cd.Aplica = m.Mov AND m.Modulo = 'COMS' AND m.Clave = 'COMS.O'
      JOIN Compra c ON cd.Aplica = c.Mov AND cd.AplicaID = c.MovID AND c.Empresa = @Empresa
   	 WHERE cd.ID = @ID
   	 GROUP BY c.ID, cd.Aplica, cd.AplicaID, cd.Articulo 
   	
   	OPEN crCompraDVenta
   	FETCH next FROM crCompraDVenta INTO @CompraID, @Articulo, @Cantidad
   	WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
   	BEGIN
   		EXEC spDicoCompraVentaActualizar @Empresa, @CompraID, @Articulo, @Cantidad, @Ok OUTPUT, @OkRef OUTPUT
   		
   		FETCH next FROM crCompraDVenta INTO @CompraID, @Articulo, @Cantidad
   	END
   	
   	CLOSE crCompraDVenta
   	DEALLOCATE crCompraDVenta
  	 
  END

  -- Devolucion Venta, Recibo Traspaso, Transferencia y Recibo Cambio Fisico, Entrada de Compra y Recibo Cambio Fisico, generan Pedido Saldo y Pedido Margen
  IF (@MovTipo in ('VTAS.D', 'INV.EI', 'INV.T', 'COMS.F', 'COMS.EG', 'COMS.EI') OR @Mov = 'Recibo Cambio Fisico') AND @Estatus in ('SINAFECTAR') AND @EstatusNuevo = 'CONCLUIDO' AND @Mov <> 'Entrada Compra Menor'
  BEGIN
    IF @Modulo = 'VTAS' SELECT @Almacen = Almacen FROM Venta  WHERE ID = @ID ELSE
    IF @Modulo = 'INV'
    BEGIN
    	SELECT @Almacen = AlmacenDestino FROM Inv WHERE ID = @ID
    	
    	SELECT @OrigenTipo = i2.OrigenTipo, @Origen = i2.Origen, @OrigenID = i2.OrigenID
        FROM Inv i
        JOIN MovFlujo mf ON i.ID = mf.DID AND mf.DModulo = 'INV' AND mf.OModulo = 'INV' --AND mf.Cancelado = 0
        JOIN MovFlujo mf2 ON mf.OID = mf2.DID AND mf2.DModulo = 'INV' AND mf2.OModulo = 'INV' AND mf2.Cancelado = 0
        JOIN MovFlujo mf3 ON mf2.OID = mf3.DID AND mf3.DModulo = 'INV' AND mf3.DModulo = 'INV' AND mf3.Cancelado = 0
        JOIN Inv i2 ON mf3.OID = i2.ID
       WHERE i.ID = @ID
    END
    ELSE
    IF @Modulo = 'COMS'
    BEGIN
      SELECT @MovTipoOrigen = NULL, @Origen = NULL
      SELECT @Almacen = Almacen, @Origen = Origen FROM Compra WHERE ID = @ID
      SELECT @MovTipoOrigen = Clave FROM MovTipo WHERE Modulo = 'COMS' AND Mov = @Origen
      IF @MovTipoOrigen = 'COMS.CC'
        SELECT @Almacen = NULL

    END
    ELSE
    SELECT @Almacen = null

    IF @Almacen is not null AND (SELECT Grupo FROM Alm WHERE Almacen = @Almacen) = 'CEDI' AND NOT(@MovTipo = 'INV.EI' AND @OrigenTipo = 'VTAS' AND @Origen = 'Pedido')
      EXEC spDicoVentaGeneraPedido @Empresa, @Sucursal, @Modulo, @ID, @Estatus, @EstatusNuevo, @Usuario, @FechaEmision, @FechaRegistro,
                          @Mov, @MovID, @MovTipo, @Ok OUTPUT, @OkRef OUTPUT
  END

  IF @Modulo = 'CXC' AND @EstatusNuevo in ('PENDIENTE', 'CONCLUIDO') AND @Ok = 80030
    SELECT @Ok = NULL

  -- Enrique Toral 1/Ene/2019 - Al Afectar/Cancelar Pedido, Reservado, Factura Piso que viene de cotización, Concluir/Activar la cotización
  IF @Modulo = 'VTAS' AND @MovTipo IN ('VTAS.P', 'VTAS.FM') AND 
     ((@Estatus = 'SINAFECTAR' AND @EstatusNuevo IN ('PENDIENTE', 'PROCESAR')) OR (@Estatus IN ('PENDIENTE', 'PROCESAR', 'CONCLUIDO') AND @EstatusNuevo = 'CANCELADO')) AND @Ok IS NULL
  BEGIN
  	SELECT @OrigenTipo = OrigenTipo, @Origen = v.Origen, @OrigenID = v.OrigenID
  	FROM Venta v
  	WHERE v.ID = @ID
  	
  	IF @OrigenTipo = 'VTAS'
  	BEGIN
  		IF (SELECT mt.Clave FROM MovTipo mt WHERE mt.Modulo = 'VTAS' AND mt.Mov = @Origen) = 'VTAS.C'
  		BEGIN
  			IF @EstatusNuevo = 'CANCELADO'
  			  SELECT @EstatusActualizar = 'CONFIRMAR'
  			ELSE
  				SELECT @EstatusActualizar = 'CONCLUIDO'
  				
  		  SELECT @IDAplica = ID
  		    FROM Venta
  		   WHERE Empresa = @Empresa AND Mov = @Origen AND MovID = @OrigenID
  		   
  		  IF @IDAplica IS NOT NULL
  		  BEGIN
  				
  			  UPDATE Venta SET Estatus = @EstatusActualizar
  			   WHERE ID = @IDAplica
    			 
  			  -- INSERT INTO MovTiempo(Modulo, ID, FechaComenzo, Estatus, Usuario)
  			  -- VALUES(@Modulo, @IDAplica, GETDATE(), @EstatusActualizar, @Usuario)
        END
  		END
  		
  	END
  	
  	-- Enrique Toral 6/Ene/2019 - Al Afectar Pedido con Artículo con ciclo Piso, genera orden transferencia
  	IF @Mov = 'Pedido' AND @Estatus = 'SINAFECTAR' AND @EstatusNuevo = 'PENDIENTE' AND @Ok IS NULL
  	BEGIN
  		IF EXISTS(SELECT d.ID
  		            FROM VentaD d
  		            JOIN Art a ON d.Articulo = a.Articulo
  		           WHERE d.ID = @ID
  		             AND a.Tipo NOT IN ('SERVICIO', 'JUEGO')
  		             AND a.DicoCiclo = 'PISO')
  		BEGIN
  			SELECT @InvMov = InvOrdenTraspaso
  			  FROM EmpresaCfgMov ecm
  			 WHERE ecm.Empresa = @Empresa
  			 
  			SELECT @Almacen = s.AlmacenPrincipal FROM Sucursal s WHERE s.Sucursal = @Sucursal
  			
  			INSERT INTO Inv(
  				       Empresa,  Mov,      FechaEmision,   Moneda,   TipoCambio,  Usuario, Referencia,                          Estatus,       Almacen, AlmacenDestino, OrigenTipo, Origen, OrigenID, Sucursal, SucursalOrigen, SucursalDestino)
  			SELECT v.Empresa, @InvMov, v.FechaEmision, v.Moneda, v.TipoCambio, @Usuario, RTRIM(v.Mov) + ' ' + RTRIM(v.MovID), 'SINAFECTAR', @Almacen, Almacen,        'VTAS',     Mov,    MovID,    Sucursal, Sucursal, Sucursal
  			  FROM Venta v
  			 WHERE ID = @ID
  			 
  			SELECT @InvID = IDENT_CURRENT('Inv')
  			
  			IF @InvID IS NOT NULL
  			BEGIN
  				INSERT INTO InvD(ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, Articulo, Unidad, Factor, Almacen, Sucursal, SucursalOrigen)
  				SELECT @InvID, d.Renglon, d.RenglonSub, d.RenglonID, d.RenglonTipo, d.Cantidad, d.Articulo, d.Unidad, d.Factor, @Almacen, d.Sucursal, d.SucursalOrigen
  				  FROM VentaD d
  		      JOIN Art a ON d.Articulo = a.Articulo
  		     WHERE d.ID = @ID
  		       AND a.Tipo NOT IN ('SERVICIO', 'JUEGO')
  		       AND a.DicoCiclo = 'PISO'
  		       
  		    EXEC spAfectar 'INV', @InvID, 'AFECTAR', NULL, NULL, @Usuario, 0, 1, @Ok OUTPUT, @OkRef OUTPUT, NULL, 1
  			END
  		END
  	END
  END
  
  -- Enrique Toral 6/Ene/2019 - Al Recibir Traspaso originado por Pedido con Artículos con Ciclo Piso, Reserva Pedido
  IF @Modulo = 'INV' AND @MovTipo = 'INV.EI' AND @Estatus = 'SINAFECTAR' AND @EstatusNuevo = 'CONCLUIDO'
  BEGIN
  	SELECT @OrigenTipo = i2.OrigenTipo, @Origen = i2.Origen, @OrigenID = i2.OrigenID
      FROM Inv i
      JOIN MovFlujo mf ON i.ID = mf.DID AND mf.DModulo = 'INV' AND mf.OModulo = 'INV' --AND mf.Cancelado = 0
      JOIN MovFlujo mf2 ON mf.OID = mf2.DID AND mf2.DModulo = 'INV' AND mf2.OModulo = 'INV' AND mf2.Cancelado = 0
      JOIN MovFlujo mf3 ON mf2.OID = mf3.DID AND mf3.DModulo = 'INV' AND mf3.DModulo = 'INV' AND mf3.Cancelado = 0
      JOIN Inv i2 ON mf3.OID = i2.ID
     WHERE i.ID = @ID

    IF @OrigenTipo = 'VTAS' AND @Origen = 'Pedido'
    BEGIN
    	SELECT @VentaID = ID
    	  FROM Venta v
    	 WHERE v.Empresa = @Empresa AND v.Mov = @Origen AND v.MovID = @OrigenID

    	IF @VentaID IS NOT NULL
    	BEGIN
    		UPDATE VentaD SET CantidadA = NULL WHERE ID = @VentaID
    		
    		UPDATE VentaD SET CantidadA = id.Cantidad
    		  FROM InvD id
    		 WHERE VentaD.ID = @VentaID
    		   AND ISNULL(VentaD.CantidadReservada, 0) = 0
    		   AND id.ID = @ID AND VentaD.Renglon = id.Renglon AND VentaD.RenglonSub = id.RenglonSub AND VentaD.RenglonID = id.RenglonID AND VentaD.Articulo = id.Articulo

        IF EXISTS(SELECT ID FROM VentaD WHERE ID = @VentaID AND CantidadA IS NOT NULL)
          EXEC spAfectar 'VTAS', @VentaID, 'RESERVAR', 'SELECCION', NULL, @Usuario, 0, 1, @Ok OUTPUT, @OkRef OUTPUT, NULL, 1

    	END
    END
  END

  RETURN
END
GO