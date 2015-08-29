##citim fisierul
data<-read.csv("MyAvis_OT_FINAL formated date.csv")
##datele sunt in format an-luna-zi !

##calculez pe 6 trimestre (1 an jumatate)
q1<-0
q2<-0
q3<-0
q4<-0
q5<-0
q6<-0

##in clean_data tinem tabelul calculat cu q-uri

nrow(data)
clean_data<-matrix(NA,nrow(data),7)

i<-1
j<-0 ## cu j avansam in citirile contorului
k<-0 ## in k tinem pozitia din noul tabel creat in clean_data
counter<-0 ##conditie de oprire cand counter=6
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
        l<-i ## cu l ne tinem dupa j (pentru consum trebuie 2 citiri consecutive)
        counter<-0
        ##in counter tinem cate quartere am calculat
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
                ##tratam cazul citit la 3 luni
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
                        ##tratam cazul citit la jumatate de an
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
                        ##tratam cazul citit la un an
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
                                counter<-counter+3
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

## salvam in csv
write.csv(clean_data,"date suspecti.csv")
