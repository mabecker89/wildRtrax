
```{sql, connection=con, output.var="new"}

select task_id, organization_full_name, project_full_nm, user_name, species_code, species_english_name, abundance_type, confidence_type, method_type, status_task_type, task_rain_id, task_industrial_noise_id, task_wind_id, task_other_noise_id, recording_file_path, recording_source_file_path as original_source, recording_file_name, project_year, aru.get_tag_start_time(tag_id) as tag_start_time, aru.get_tag_length(tag_id) as tag_length
from aru.tag
join aru.species_individual on tag_species_individual_id = si_id
join aru.task on si_task_id = task_id
join aru.lu_species on si_species_code = species_code
join aru.lu_abundance on si_abundance_id = abundance_id
join aru.recording on task_recording_id = recording_id
join aru.lu_status_task on status_task_id = task_status_id
join aru.lu_confidence on si_confidence_id = confidence_id
join aru.lu_method on task_method_id = method_id
join common.project on task_project_id = project_id
join common.app_user on task_transcriber_user_id = user_id
join common.organization on project_organization_id = organization_id
where project_full_nm ~* 'ABMI';

```

```{r}

new$original_source<-as.character(new$original_source)
new$tag_end_time<-new$tag_start_time+new$tag_length
new$tag_start_time<-new$tag_start_time-5
new$tag_end_time<-new$tag_end_time+5
new$tag_start_time<-ifelse(new$tag_start_time<0.00,0,new$tag_start_time)
new$tag_end_time<-ifelse((new$tag_end_time>120) & (str_detect(new$method_type,"2m")),120,
                         ifelse((new$tag_end_time>180) & (str_detect(new$method_type,"3m")),180,
                                ifelse((new$tag_end_time>300) & (str_detect(new$method_type,"5m")),300,
                                       ifelse((new$tag_end_time>360) & (str_detect(new$method_type,"6m")),360,
                                              ifelse((new$tag_end_time>60) & (str_detect(new$method_type,"1m")),60,
                                                     ifelse((new$tag_end_time>600) & (str_detect(new$method_type,"10m")),600,
                                                            new$tag_end_time))))))
new$original_source<-sub("media", "Volumes", new$original_source)
new$original_source<-sub("BUdata01", "BUdata", new$original_source)
new$original_source<-sub("BUdata02", "BUdata", new$original_source)
new$original_source<-sub("BUdata03", "BUdata", new$original_source)
new$ext<-str_replace(str_extract(new$original_source,'\\.(.*)'),'.','')
new=new[!duplicated(new$original_source),]

for (i in 1:nrow(new)) {
  writedir = '/users/alexandremacphail/desktop/testwav'
  tmp <- tryCatch(
    (if (new$ext[i]=='wac') {
      read_wac(new$original_source[i])
    } else if (new$ext[i]=='wav') {
      readWave(new$original_source[i],from=new$tag_start_time[i],to=new$tag_end_time[i],units="seconds")
    } else {print('Could not read ', new$original_source, ' or source not valid')
    }),
    error=function(e) {
      msg<-conditionMessage(e)
      print(paste0(msg, new$recording_file_name[i], sep=' '))}
  )
  outfile<-sub("^(.*?)\\.", paste(new$species_code[i],"-",new$task_id[i],"-",round(new$tag_start_time[i],0),"-",new$user_name,".",sep=""), new$original_source)
  outpath<-file.path(writedir, basename(outfile))
  writefiles <- tryCatch(
    writeWave(tmp, filename=outpath[i], extensible=TRUE),
    error=function(f) {
      msg2<-conditionMessage(f)
      print(paste0(msg2, new$recording_file_name[i], sep=' '))
    })
}


```
