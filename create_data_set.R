##reading the file
data<-read.csv("x.csv")
##the dates are in year-month-day format

##ccalculating consumtion for 1.5 year (6 trimesters)
q1<-0
q2<-0
q3<-0
q4<-0
q5<-0
q6<-0

##clean_data will hold the new quarterly consumption
nrow(data)
clean_data<-matrix(NA,nrow(data),7)

##with i we advance in meter ID
i<-1
##with j we advance inside meter readings
j<-0 
##in k we count current ID position
k<-0 
##counter stops program when we already calculated 6 quarterly consumptions
counter<-0 

while(i<nrow(data))
{
        if(i%%1000)print(i)
        k<-k+1
        j<-i+1
        clean_data[[k,1]]<-paste(data$ID[i])
        q1<-0
        q2<-0
        q3<-0
        q4<-0
        q5<-0
        q6<-0    
        ##l advances one step ahead in redings (for the consumption calculation)
        l<-i 
        counter<-0
        while(paste(data$ID[i])==paste(data$ID[j]))
        {
                date1<-as.POSIXct(data[l,5])
                date2<-as.POSIXct(data[j,5])
                
                daysdiff<-date1-date2
                qdif<-abs(daysdiff-90)
                hdif<-abs(daysdiff-180)
                ydif<-abs(daysdiff-360)
                dif<-min(qdif,hdif,ydif)
                print(daysdiff)
                print(qdif)
                print(hdif)
                print(ydif)
                print(dif)
                print("--------------")
              
                ##the case when we have quarterly readingds (each 3 months)
                if(qdif==dif)
                {
                        if(counter<6)
                        {
                                q1<-q2
                                q2<-q3
                                q3<-q4
                                q4<-q5
                                q5<-q6
                                q6<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)
                                
                        }
                        counter<-counter+1
                }
                else 
                       ## the case when we have 2 readings a year
                        if(hdif==dif)
                        {
                                if (counter<6)
                                {
                                        q1<-q3
                                        q2<-q4
                                        q3<-q5
                                        q4<-q6
                                        q5<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.5
                                        q6<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.5
                                        
                                }
                                counter<-counter+2
                        }
                else
                       ## the case when we have only one readings a year
                        if(ydif==dif)
                        {
                                if(counter<6)
                                {
                                        q1<-q2
                                        q2<-q3
                                        q3<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.25
                                        q4<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.25
                                        q5<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.25
                                        q6<-(as.numeric(data$Index[j])-as.numeric(data$Index[l]))/as.numeric(date2-date1)*0.25
                                        
                                }  
                                counter<-counter+4
                        }
                
                l<-l+1
                j<-j+1
                
        }
        clean_data[[k,2]]<-q1
        clean_data[[k,3]]<-q2
        clean_data[[k,4]]<-q3
        clean_data[[k,5]]<-q4
        clean_data[[k,6]]<-q5
        clean_data[[k,7]]<-q6
        i<-j
}

##save the resulting file
write.csv(clean_data,"y.csv")
