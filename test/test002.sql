SELECT NombreProducto, Precio FROM "Productos"
WHERE Precio - (SELECT AVG(Precio) FROM Productos);