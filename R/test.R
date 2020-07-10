#How to mess around with stuff, microbenchmarking etc.


tic()
test<-wt_aru_scanner('/volumes/budata/abmi/2015/01/ABMI-0033/ABMI-0033-SE','\\.wac$') #wacs
toc()

tic()
test<-wt_aru_scanner('/volumes/budata/abmi/2019/01/ABMI-0509','\\.wav$|\\.wac$') #wavs
toc()


tic()
test<-wt_aru_scanner('/users/alexandremacphail/desktop/testwav-1','\\.wac$|\\.wav$') #wacs
toc()

tic()
mcfn<-scanner('/volumes/budata/notproofed/2019/bu-contracts/mcfn','\\.wav$|\\.wac$')
toc()

tic()
x2<-wt_aru_assign(xAB, blocks="ABMI_stratified")
toc()

x2[[1]]
x2[[2]]

o<-read.csv('/users/alexandremacphail/desktop/OWGR.csv')

nu<-read.csv('/users/alexandremacphail/desktop/nuwcru_assign_full.csv')

for (i in 1:nrow(nu)) {

  system2(command)
}
bod<-'/Volumes/BUdata/wildtrax_loads'
command <- sprintf('sudo ln -sf "%s" "%s" ', unlist(nu$filepath), bod)

system2('/Volumes/BUdata/BU-Contracts/LTEMP',command)
plot


xa<-x %>%
  filter(filepath %in% o$filepath) %>%
  top_n(., 20, station_key)

mcfndays<-mcfn %>%
  filter(Time_index %in% c(2,3)) %>%
  filter(Julian_Date<190) %>%
  group_by(Station) %>%
  sample_n(4, replace=T)

mcfnnights<-mcfn %>%
  filter(Time_index %in% c(14,15)) %>%
  filter(Julian_Date<190) %>%
  group_by(Station) %>%
  sample_n(4, replace=T)

mcfn2<-rbind(mcfndays,mcfnnights)


wt_link<-function(df) {
  for (i in 1:nrow(df)) {
  base_output_directory[i] <- paste0('/Volumes/BUdata/wildtrax_loads','/',basename(df$filepath[i]))
  message('*** Creating symlink to ', base_output_directory[i], ' ***')
  # prepare command
  command <- sprintf(' sudo ln -s "%s" "%s" ', df$filepath[i], base_output_directory[i])
  # finally, execute the command
  link<-tryCatch(system2(command),
                 error=function(e){
                   msg<-conditionMessage(e)
                   print(paste0(msg2, ' for ', df$filepath[i]))
                 })
  }}

w<-readWave('/users/alexandremacphail/desktop/testwav/ABMI-0509-SE_20190319_142713.wav')
x<-read_wac('/users/alexandremacphail/desktop/testwav/ABMI-0509-SW_0+1_20190319_141314.wac')

y<-read_audio('/users/alexandremacphail/desktop/testwav/ABMI-0509-SW_0+1_20190319_141314.wac')


x<-read_audio()





