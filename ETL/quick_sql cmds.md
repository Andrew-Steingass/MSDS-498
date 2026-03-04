## DROP ALL TABLES WITHIN DB (testing)
-- Drop all foreign key constraints first
DECLARE @sql NVARCHAR(MAX) = '';

SELECT @sql += 'ALTER TABLE ' + QUOTENAME(OBJECT_SCHEMA_NAME(parent_object_id)) 
             + '.' + QUOTENAME(OBJECT_NAME(parent_object_id)) 
             + ' DROP CONSTRAINT ' + QUOTENAME(name) + ';'
FROM sys.foreign_keys;

EXEC sp_executesql @sql;

-- Drop all tables
SET @sql = '';

SELECT @sql += 'DROP TABLE ' + QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME) + ';'
FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE = 'BASE TABLE';

EXEC sp_executesql @sql;

