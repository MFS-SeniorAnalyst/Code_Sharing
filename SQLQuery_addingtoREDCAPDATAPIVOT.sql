
USE [db_name] 
INSERT INTO [dbo].[PivotProjects] 
([project_id]
,[project_name]
,[pivot_with_raw_data]
,[multi_column_pivot]
,[pivot_with_timestamp]
,[username])
VALUES
('PID'
,'nameoftable'
,1
,1
,1
,'your_username')
SELECT * FROM PivotProjects
