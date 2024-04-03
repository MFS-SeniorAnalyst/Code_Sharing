USE [RedCapDataPivot]

INSERT INTO [dbo].[PivotProjects]
           ([project_id]
           ,[project_name]
           ,[pivot_with_raw_data]
           ,[multi_column_pivot]
           ,[pivot_with_timestamp]
           ,[username])
     VALUES
           ('436'
           ,'MPA Participants'
           ,1
           ,1
           ,1
           ,'apearson')

SELECT * from [dbo].[PivotProjects]