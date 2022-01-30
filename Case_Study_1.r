# Reading the csv file with data of Apac Business Schools
BSchools <- read.csv("C:\\MSBA\\BAN-602\\Case_Studies\\Case_Study_1\\BAN_602_Case_1.csv")

# Question-1
# selecting only the quantitative variables from the BSchools data frame.
Quant_Vars <- (BSchools[c(2,3,4,5,6,7,11)])

# Calculating number of Foreign Students from %Foreign column 
Foreign_Students <- (round((as.numeric(unlist(Quant_Vars$Full.Time.Enrollment))*
                     as.numeric(unlist(Quant_Vars$X.Foreign)))/100)
                     )

# Replacing %Foreign column with Foreign Students column
Quant_Vars[6] <- Foreign_Students

# Calculating number of Local Students
Local_Students <- (as.numeric(unlist(Quant_Vars$Full.Time.Enrollment))-as.numeric(unlist(Quant_Vars$X.Foreign)))

# Appending Local Students column into data frame
Quant_Vars <- cbind(Quant_Vars,Local_Students)

# creating a vector of length of  Quant_Vars data frame.
ir <- 1:length(Quant_Vars)

# creating a header row vector
header <- c('Details','Full_Time_Enrollment','Students_per_Faculty','Local_Tuition','Foreign_Tuitiion','Age','Foreign_Students','Starting_Salary','Local_Students')

# creating the details column
details <- data.frame('Details'=c('Mean','Median','Standard_Deviation','Variance','Min','Max'))

# Iterating over the Quant_Vars data frame to calculate (mean,median,sd,variance,min,max) of all columns.
for (i in ir){
  csdf <- data.frame(
    c(mean(as.numeric(gsub(",","",unlist(Quant_Vars[i])))),
    median(as.numeric(gsub(",","",unlist(Quant_Vars[i])))),
    sd(as.numeric(gsub(",","",unlist(Quant_Vars[i])))),
    var(as.numeric(gsub(",","",unlist(Quant_Vars[i])))),
    min(as.numeric(gsub(",","",unlist(Quant_Vars[i])))),
    max(as.numeric(gsub(",","",unlist(Quant_Vars[i]))))
    )
  )
  details <- cbind(details,csdf)
}

# Creating the final data frame
final_df <- rbind(header,details)

# Adding the header row in the final data frame
colnames(final_df)<-header

# Question-2

#2A
# Difference between Local & Foreign Tution
Avg_Local_Tution <- mean(as.numeric(gsub(",","",unlist(BSchools$Local.Tuition....))))
Avg_Forigen_Tution <- mean(as.numeric(gsub(",","",unlist(BSchools$Foreign.Tuitiion....))))

# Graphical representation of the difference between Local & Foreign Tution
plot(as.numeric(gsub(",","",unlist(BSchools$Local.Tuition....))), type = "b", pch = 19, 
     col = "red",xlab = "College Records" ,ylab = "Local Tuition Cost")
lines(as.numeric(gsub(",","",unlist(BSchools$Foreign.Tuitiion....))),  pch = 18, col = "blue", type = "b", lty = 2)
legend("topleft", legend=c("Local Tuition($)", "Foreign Tuition($)"),
       col=c("red", "blue"), lty = 1:2, cex=0.6)

# Numerical representing of the difference between Local & Foreign Tution
diff_al_af_tution <- round(((Avg_Forigen_Tution - Avg_Local_Tution)/Avg_Local_Tution)*100)
print(diff_al_af_tution)

#2B
# Difference between mean starting salary for schools with and without work experience.
mean_ss_wex <- mean(as.numeric(gsub(",","",subset(BSchools$Starting.Salary....,BSchools$Work.Experience == 'Yes'))))
mean_ss_nwex <- mean(as.numeric(gsub(",","",subset(BSchools$Starting.Salary....,BSchools$Work.Experience == 'No'))))

# Graphical representation
barplot(c(mean_ss_wex,mean_ss_nwex),
        main = "Mean of Starting Salary with and without work experience",
        xlab = "Starting Salary",
        col = c("red","green"))
legend("topleft",
       c("With Work Exp","Without Work Exp"),
       fill = c("red","green"), cex = 0.7)

# Numerical representation
diff_ss_wex_nwex <- round(((mean_ss_wex - mean_ss_nwex)/mean_ss_nwex)*100)
print(diff_ss_wex_nwex)

#2C
# Difference between mean starting salary for schools with and without english tests.
mean_ss_et <- mean(as.numeric(gsub(",","",subset(BSchools$Starting.Salary....,BSchools$English.Test == 'Yes'))))
mean_ss_net <- mean(as.numeric(gsub(",","",subset(BSchools$Starting.Salary....,BSchools$English.Test == 'No'))))

# Graphical representation
barplot(c(mean_ss_et,mean_ss_net),
        main = "Mean of Starting Salary with and without english test",
        xlab = "Starting Salary",
        col = c("red","green"))
legend("topleft",
       c("With Work Exp","Without Work Exp"),
       fill = c("red","green"), cex = 0.7)
# Numerical representation
diff_ss_et_net <- round(((mean_ss_et - mean_ss_net)/mean_ss_net)*100)
print(diff_ss_et_net)

# Question-3

# Converting the Starting_Salary, Local and Foreign Tution columns into numerical vectors. 
ss <- as.numeric(gsub(",","",unlist(BSchools$Starting.Salary....)))
lt <- as.numeric(gsub(",","",unlist(BSchools$Local.Tuition....)))
ft <- as.numeric(gsub(",","",unlist(BSchools$Foreign.Tuitiion....)))

# Calculating the Corelation between Starting_Salary and Local_Tution
cor_s_lt <- cor(ss,lt)
# Numerical representation
print(cor_s_lt)
# Graphical representation
plot(ss,lt)

# Calculating the Corelation between Starting_Salary and Foreign_Tution
cor_s_ft <- cor(ss,ft)
# Numerical representation
print(cor_s_ft)
# Graphical representation
plot(ss,ft)

# Question-4

# Graphical representation of Starting_Salary with boxplot
boxplot(x=ss, horizontal = TRUE,main="Question 4", ylab="Starting Salary",axes = FALSE)
text(x=fivenum(as.numeric(bpStartSal)), labels =fivenum(ss), y=1.3)
grid(nx=16, ny=16)

stripchart(ss, method = "jitter", pch = 19, add = TRUE, col = "blue")

# (Q1 - 1.5 * IQR or Q3 + 1.5 * IQR)
interQR = 52500 - 16000 # calculating inter quartile range
outlierUpper = 52500 + (1.5*interQR) # upper limit for outlier
outlierLower = 16000 - (1.5*interQR) # lower limit for outlier

#Checking for outliers in both directions

if (outlierUpper > max(ss)) {
  print('There are no outliers above')
}

if (outlierUpper < min(ss)) {
  print('There are no outliers below')
}