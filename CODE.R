# Determining if UCB has discriminated against female applicants

# load packages
require(dplyr)
require(reshape2)
require(ggplot2)
require(tibble)
require(readr)

#ciu is a code to clean up the ggplots contained below
ciu = theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "grey"))

# loading the data
ucb <- read_csv("ucb_admissions.csv")

#UNIVERSITY ANALYSIS ####
# THIS ANALYSIS IS TO SHOW WHEITHER THE UCB HAD DISCRIMINATED AGAINST THEIR FEMALE APPLICANTS

#UNIVERSITY SUMMARY
#BAR CHART SHOWING RECIEVED APPLICATIONS FROM BOTH GENDERS
ucbbar = ggplot(ucb, aes(Gender, Freq))
ucbbar +
  geom_bar(stat = "identity") +
  ylim(0,3000) +
  labs(title = "Amount of Recieved Applications", subtitle = "by Gender",
       caption = "Female: 1835 \n Male::2691") +
  xlab("Gender") +
  ylab("# of Applicants") +
  ciu
  
#BAR CHART SHOWING THE PERCENTAGES OF APPLICANTS
#table created
gender <- ucb %>%
  group_by(Gender) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(Ratio=Freq/sum(Freq)) %>% view()

#bar graph
genderbar = ggplot(gender, aes(x=Gender, y=Ratio*100))
genderbar +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Ratio*100, digits = 2),"%")), 
            position = position_stack(vjust = 0.5), size = 4.5, color = "white") +
  labs(title = "Gender Percentage of Applicants",
       caption = "2:3 ratio of Female to Male Applicants") +
  ylab("%") +
  ciu


#BAR CHART SHOWING TOTAL MEN AND WOMEN WHO APPLIED SEPERATED BY ADMITTED AND REJECTED
ucbbar = ggplot(ucb, aes(x=Gender, y=Freq, fill=Admit))
ucbbar +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title = "Applications Submitted Throughout University", subtitle = "Admitted vs Rejected", 
       fill="Admitted vs Rejected") +
  xlab("Gender") +
  ylab("# of Applicants") +
  ciu

#GENDER SUMMARY ####
#THE FOLLOWING TABLES SHOWS THE ADMISSIONS STATUS RATIOS FOR WOMEN AND MEN

#WOMEN ADMISSIONS TABLE RATIO
fucb <- ucb %>% 
  filter(is.element(Gender, c("Female"))) %>% 
  group_by(Admit) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(Ratio = Freq/sum(Freq))

#WOMEN ADMISSIONS GRAPH
fucbbar = ggplot(fucb, aes(x=Admit, y=Ratio*100))
fucbbar +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Ratio*100, digits = 2),"%")), 
            position = position_stack(vjust = 0.5), size = 4.5, color = "white") +
  labs(title = "Female Applicants Admissions Status", subtitle = "Admitted vs Rejected") +
  xlab("Gender") +
  ylab("%") +
  ciu

fucbbar1 = ggplot(fucb, aes(x=Admit, y=Freq))
fucbbar1 +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.5), size = 6, color = "White") +
  labs(title = "Female Status Count") +
  xlab("Status") +
  ylab("count") +
  ciu

#MEN ADMISSIONS RATIO TABLE
mucb <- ucb %>%   
  filter(is.element(Gender, c("Male"))) %>%
  group_by(Admit) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(Ratio = Freq/sum(Freq)) %>% view()

#MEN ADMISSIONS RATIO GRAPH
mucbbar = ggplot(mucb, aes(x=Admit, y=Ratio*100))
mucbbar +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Ratio*100, digits = 2),"%")), 
            position = position_stack(vjust = 0.5), size = 4.5, color = "white") +
  labs(title = "Male Applicants Admissions Status", subtitle = "Admitted vs Rejected") +
  xlab("Status") +
  ylab("%") +
  ciu

#MEN ADMISSIONS COUNT GRAPH
mucbbar1 = ggplot(mucb, aes(x=Admit, y=Freq))
mucbbar1 +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.5), size = 6, color = "White") +
  labs(title = "Male Status Count") +
  xlab("Status") +
  ylab("count") +
  ciu


#DEPARTMENT SUMMARY####

#DEPARTMENT TABLE
dept <- ucb %>%
  group_by(Dept,
           Gender) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(Ratio=Freq/sum(Freq)) %>% view()

#APPLICANTS COUNT TO DEPARTMENT BY GENDER
deptucbbar = ggplot(dept, aes(x=Dept, y=Freq, fill=Gender))
deptucbbar +
  geom_bar(stat = "identity") +
  labs(title = "Submitted Applications to Department", subtitle = "by Gender") +
  coord_flip() +
  xlab("Department") +
  ylab("# of Applications") +
  ciu

#APPLICANT RATIOS TO DEPARTMENT BY GENDER
deptucbbar1 = ggplot(dept, aes(x=Dept, y=Ratio*100, fill=Gender))
deptucbbar1 +
  geom_bar(stat = "identity") +
  labs(title = "Submitted Applications to Department", subtitle = "Gender Ratio") +
  geom_text(aes(label = paste(round(Ratio*100, digits = 2),"%")), 
            position = position_stack(vjust = 0.5), size = 4.0, color = "white") +
  coord_flip() +
  xlab("Department") +
  ylab("%") +
  ciu


#SUMMARY BY INDIVIDUAL DEPARTMENTS
# DEPARTMENT A TABLE
ucbDeptA <- ucb %>% filter(is.element(Dept, c("A"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT A GRAPH
ucbDeptAbar = ggplot(ucbDeptA, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptAbar +
  geom_bar(stat = "identity") +
  labs(title = "Department A") +
  ciu

# DEPARTMENT B TABLE
ucbDeptB <- ucb %>% filter(is.element(Dept, c("B"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT B GRAPH
ucbDeptBbar = ggplot(ucbDeptB, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptBbar +
  geom_bar(stat = "identity") +
  labs(title = "Department B") +
  ciu

# Department C
ucbDeptC <- ucb %>% filter(is.element(Dept, c("C"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT C GRAPH
ucbDeptCbar = ggplot(ucbDeptC, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptCbar +
  geom_bar(stat = "identity") +
  labs(title = "Department C") +
  ciu

# Department D
ucbDeptD <- ucb %>% filter(is.element(Dept, c("D"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT D GRAPH
ucbDeptDbar = ggplot(ucbDeptD, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptDbar +
  geom_bar(stat = "identity") +
  labs(title = "Department D") +
  ciu

# Department E
ucbDeptE <- ucb %>% filter(is.element(Dept, c("E"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT E GRAPH
ucbDeptEbar = ggplot(ucbDeptE, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptEbar +
  geom_bar(stat = "identity") +
  labs(title = "Department E") +
  ciu

# Department F
ucbDeptF <- ucb %>% filter(is.element(Dept, c("F"))) %>% group_by(Gender, Dept) %>% 
  mutate(Ratio=Freq/sum(Freq)) %>% view()
# DEPARTMENT F GRAPH
ucbDeptFbar = ggplot(ucbDeptF, aes(x=Gender, y=Freq, fill=Admit))
ucbDeptFbar +
  geom_bar(stat = "identity") +
  labs(title = "Department F") +
  ciu

