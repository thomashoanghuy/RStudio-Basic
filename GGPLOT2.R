library(tidyverse)
Exam_data <- read.csv("C:/Users/user/Desktop/School/ISSS608 Visual Analytics and Applications/LESSON 1/Exam_data.csv")
ggplot(data = Exam_data, aes(x = MATHS))+
  geom_histogram(bins=10,
                 boundary =100,
                 color = "black" ,
                 fill = "grey") + 
  ggtitle("Distribution of Maths Scores")

ggplot(data = Exam_data, aes(x = RACE))+
  geom_bar()

ggplot(data = Exam_data, aes(x = MATHS))+
  geom_dotplot(bindwidth = 2.5 , dotsize = 0.5)+
  scale_y_continuous(NULL, breaks = NULL)

ggplot(data = Exam_data, aes(x = MATHS))+
  geom_histogram(bins=20,
                 color = "black" ,
                 fill = "light blue") + 
  ggtitle("Distribution of Maths Scores")

ggplot(data = Exam_data, aes(x = MATHS, fill = GENDER))+
  geom_histogram(bins=20,
                 color = "grey30" ,)
  ggtitle("Distribution of Maths Scores")
  
ggplot(data = Exam_data, aes (x = MATHS, colour = GENDER)) +
  geom_density()

ggplot(data=Exam_data, aes(y = MATHS, x= GENDER)) +
  geom_boxplot(notch = TRUE)

ggplot(data=Exam_data, aes(y = MATHS, x= GENDER)) +
  geom_boxplot()
    geom_point(position = "jitter" , size = 0.5)
    
ggplot(data=Exam_data, aes(y = MATHS, x= GENDER)) +
  geom_violin( fill = "light blue") +
  geom_boxplot(alpha = 0.5)

ggplot(data = Exam_data, aes (x = MATHS , y = ENGLISH))+
  geom_point()


ggplot(data=Exam_data, aes(y = MATHS, x= GENDER)) +
  geom_boxplot()+
  stat_summary(geom = "point" ,
               fun.y = "mean",
               colour = "red",
               size = 5)
ggplot(data=Exam_data,
       aes(x= MATHS,
           y=ENGLISH)) +
  geom_point() +
  geom_smooth(method=lm,
              size= 0.5)

ggplot(data=Exam_data,
       aes(x= MATHS)) +
  geom_histogram(bins= 20) +
  facet_wrap(~ CLASS)


ggplot(data=Exam_data,
       aes(x= MATHS)) +
  geom_histogram(bins=20) +
  facet_grid(~ CLASS)

ggplot(data = Exam_data , aes(y = MATHS, x = CLASS))+
  geom_boxplot() + 
  facet_grid(~GENDER)

ggplot(data = Exam_data , aes(y = MATHS, x = CLASS))+
  geom_boxplot() + 
  facet_grid(GENDER~.)

ggplot(data = Exam_data , aes(y = MATHS, x = GENDER))+
  geom_boxplot() + 
  facet_grid(GENDER~CLASS)

ggplot(data=Exam_data,
       aes(x=RACE)) +
  geom_bar() +
  coord_flip()
  theme_gray()

ggplot(data=Exam_data,
       aes(x= MATHS,
           y=ENGLISH)) +
  geom_point() +
  geom_smooth(method=lm,
              size= 0.5)+
  coord_cartesian(xlim = c(0 , 100),
                  ylim = c(0 , 100))

ggplot(data=Exam_data,
       aes(x=RACE)) +
  geom_bar(fill = "dark green" , color = "black") +
  coord_flip() + 
  #theme_gray()
  #theme_classic()
  theme_minimal()+
  theme(panel.background = element_rect(fill = "light blue",
                                        colour = "light blue",
                                        size = 0.5,
                                        linetype = "solid"),
        panel.grid.major = element_line(size= 1.0,
                                        linetype = "solid",
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.2,
                                        linetype = "solid",
                                        colour = "white"))


Exam_data %>%
  mutate(RACE = fct_infreq(RACE)) %>%
  ggplot(aes(x = RACE)) +
  geom_bar()+
  ylim(0,220) +
  geom_text(stat="count",aes(label=paste0(..count.., ", ",
                                          round(..count../sum(..count..)*100 ,1),"%")),
            vjust=-1) +xlab("Race") +
  ylab("No. of Pupils") +
  theme(axis.title.y=element_text(angle =0 ))  



ggplot(data = Exam_data, aes (x = MATHS))+
  geom_histogram(bins = 20,
                 color = "black",
                 fill = "light blue") +
  geom_vline(aes(xintercept = mean(MATHS,na.rm = T)),
           color = "red",
           linetype = "dashed" ,
           size = 1) +
  geom_vline(aes(xintercept = median(MATHS,na.rm = T)),
             color = "blue",
             linetype = "dashed",
             size = 1)

d <- Exam_data
d_bg <- d[,-3]

ggplot(d, aes(x = ENGLISH, fill = GENDER)) +
  geom_histogram(data = d_bg, fill = "grey" , alpha= 0.5) +
  geom_histogram(colour = "black") + facet_wrap(~GENDER)+
  guides(fill = FALSE) + 
  theme_bw()


ggplot(data=Exam_data,aes(x=MATHS, y=ENGLISH)) +
  geom_point() +
  coord_cartesian(xlim=c(0,100),
  ylim=c(0,100)) +
  geom_hline(yintercept=50,linetype="dashed",color="grey60",size=1) +
  geom_vline(xintercept=50,linetype="dashed",color="grey60",size=1)
