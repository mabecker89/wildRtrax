

#Start SQL chunk; use wt_conn to connect and then run query (Marcus to re-format as dbplyr etc.)

```{sql, connection=con, output.var="taskupdate"}

with user_status as (select task_id, user_name, project_year, to_char(project_due_date,'YYYY-Mon-DD') as due_date, status_task_type, recording_file_name, to_char(recording_date,'HH24') as rec_time,
                     method_type, method_min_length, project_full_nm
                     from aru.task
                     inner join common.app_user on task_transcriber_user_id = user_id
                     inner join aru.lu_status_task on task_status_id = status_task_id
                     inner join aru.lu_method on task_method_id = method_id
                     inner join aru.recording on task_recording_id = recording_id
                     inner join common.project on task_project_id = project_id
                     where project_full_nm !~* 'Demo|Exams' and status_task_type ~* 'New|In Progress')
select task_id, project_full_nm, user_name, status_task_type, method_type, due_date
from user_status
order by 3 asc;

```
# Start R chunk
```{r}

listuserpayMonth<-c('User 1', 'User 2') #list of the users you want to check for; could make this a visibility / status within the organization

taskupdate<-taskupdate %>%
  filter(user_name %in% listuserspayMay)

taskupdate$mins<-as.numeric(str_extract(taskupdate$method_type,'\\d+(?=[a-z])'))
taskupdate$newdate <- strptime(as.character(taskupdate$due_date), "%Y-%b-%d")
taskupdate$newdate <- format(taskupdate$newdate, "%Y-%m-%d")
taskupdate<-taskupdate %>%
  group_by(user_name, project_full_nm, newdate) %>%
  dplyr::summarise(tot = sum(mins))

# Create a separate plot for each value of cyl, and store each plot in a list
update.list = lapply(sort(unique(taskupdate$user_name)), function(i) {
  ggplot(taskupdate[taskupdate$user_name==i,], aes(x=newdate, y=tot, fill=factor(project_full_nm))) +
    geom_bar(stat="identity") +
    facet_wrap(~user_name) +
    blanktheme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Due date") +
    ylab("Audio minutes") +
    ggtitle("Status summary for {user_name}") +
    scale_fill_viridis_d() +
    guides(fill=guide_legend(title="Project Name")) +
    ggsave(paste0(i,"-status_",Sys.Date(),".pdf"))
})

update.list[[1]]


```
