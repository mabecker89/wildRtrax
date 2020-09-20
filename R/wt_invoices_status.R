#Invoice function}

# Workflow
# 1) Query billable minutes from the database (calculates number of audio minutes)
# 2) Create a list of people to invoice
# 3) Filter query and apply dollar amount to billable minutes
# 4) Split the dataframe and create an invoice template for each person
# 5) Query the database for the task updates
# 6) Create a user status summary as well to track progress


wt_invoices_status <- function(users, cycle, outpath) {
  setwd(outpath)
  #Get billable minutes data
  invoices <- as_tibble(dbGetQuery(conn = con,'select * from aru.user_billable_minutes'))
  #Get task update data
  taskupdate <- as_tibble(dbGetQuery(conn = con,'
                           select "task_id",
                           "user_name",
                           "project_year",
                           to_char("project_due_date",\'YYYY-Mon-DD\') as due_date,
                           "status_task_type",
                           to_char("recording_date",\'HH24\') as rec_time,
                           "method_type",
                           "method_min_length",
                           "project_full_nm"
                           from aru.task
                           join common.app_user on "task_transcriber_user_id" = "user_id"
                           join aru.lu_status_task on "task_status_id" = "status_task_id"
                           join aru.lu_method on "task_method_id" = "method_id"
                           join aru.recording on "task_recording_id" = "recording_id"
                           join common.project on "task_project_id" = "project_id"
                           where "project_full_nm" !~* \'Demo|Exams\'
                           and "status_task_type" ~* \'New|In Progress|Review\''))
  #Filter to desired list of users
  taskupdate<-taskupdate %>%
    filter(user_name %in% users)
  #Clean up data
  taskupdate$mins<-as.numeric(str_extract(taskupdate$method_type,'\\d+(?=[a-z])'))
  taskupdate$newdate <- strptime(as.character(taskupdate$due_date), "%Y-%b-%d")
  taskupdate$newdate <- format(taskupdate$newdate, "%Y-%m-%d")
  taskupdate<-taskupdate %>%
    group_by(user_name, project_full_nm, newdate, status_task_type) %>%
    dplyr::summarise(tot = sum(mins))
  # Create a separate plot for each value and store each plot in a list
  update.list = lapply(sort(unique(taskupdate$user_name)), function(i) {
    ggplot(taskupdate[taskupdate$user_name==i,], aes(x=newdate, y=tot, fill=interaction(factor(project_full_nm),factor(status_task_type)))) +
      geom_bar(stat="identity") +
      geom_text(aes(label=status_task_type, vjust=-0.25), colour="red", size = 2) +
      facet_wrap(~user_name) +
      blanktheme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Due date") +
      ylab("Audio minutes") +
      ggtitle("Status summary for {user_name}") +
      scale_fill_viridis_d() +
      guides(fill=guide_legend(title="Project Name")) +
      ggsave(path=outpath,filename=paste0(i,"-status_",Sys.Date(),".pdf"))
  })
  #Filter and summarise invoice data
  invoicesum <- invoices %>%
    group_by(year_month,user_name,project_full_nm) %>%
    filter(user_name %in% users) %>%
    filter(year_month %in% c(cycle)) %>%
    dplyr::summarise(min_to_pay_for=sum(min_to_pay_for))
  invoicesum$dollars<-dollar(invoicesum$min_to_pay_for*1.43)
  invoicesum %>%
    group_by(user_name) %>%
    group_walk(~ write.csv(.x, paste(.y$user_name,"-invoice_",Sys.Date(),".csv",sep='')))
}

#Example
wt_inv <- wt_invoices_status(users = c("demkoad@gmail.com", "Christopher Moser-Purdy", "Jillian Slater", "Brandon Law", "bprobinson@live.ca", "s.shappas@hotmail.com", "Scott Wilson", "Christopher Wagner"),
                             cycle = '2020-08',
                             outpath = '/users/alexandremacphail/desktop/2020-2021 Listening/Invoices/Aug')

