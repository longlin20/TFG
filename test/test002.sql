SELECT NombreProducto, Precio FROM "Productos"
WHERE Precio - (select AVG(Precio) FROM Productos);