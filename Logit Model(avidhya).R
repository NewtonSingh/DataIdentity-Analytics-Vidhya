# We will upload our dataset both training and testing dataset in RStudio.

train_data = read.csv(file = "C:/Users/HP/Desktop/Dataquest Hkton/train_HK6lq50.csv")

# Now because our dataset contains factor varibles we will convert them into dummies
str(train_data)

attach(train_data)

#dummies = model.matrix(object = ~program_type+test_type+difficulty_level+gender+education+
  #  is_handicapped,data = train_data)

#head(dummies)

# Now we will check how many NAs are there in our dataset

missing_Na =  sapply(X = train_data,FUN = function(x) sum(is.na(x)))

# So there are 27729 NAs in "age" and 77 NAs in "trainee_mgmt_rating"

# Now we impute missing values in "age variable"

library(Hmisc)

train_data$age = impute(x = train_data$age,fun = median)

# Now for trainee_management rating

prop.table(table(trainee_engagement_rating))

train_data$trainee_engagement_rating = impute(x=train_data$trainee_engagement_rating
  ,fun = 1)

# Now we will combine train_data with dummies

train_data_1 = train_data

# Now we will split train_data into train set and validation set.
# We will use 95% for train and 5% for test and perform a K-cross validation

library(plyr) 
install.packages("caret")
library(caret) # Confusion matrix

# false pos rate 

fp_rate = NULL

# false negative rate

fn_rate = NULL

# Number of Iterations 

k = 20

#Now we will initialize our progress bar

p_bar =create_progress_bar("text")

p_bar$init(k)

# Accuracy

acc = NULL

set.seed(123)

for(i in 1:k)
{
  
  # Train-test Split
  
  smp_size = floor(0.95*nrow(train_data_1))
  
  index = sample(seq_len(nrow(train_data_1)),size = smp_size)
  
  train = train_data_1[index,-c(1,2)]
  test  = train_data_1[-index,-c(1,2)]
  
  # Fitting the logistic eqn
  
  model = glm(is_pass~.,family = binomial,data = train)
  
  # Predicting results
  
  results_pr = predict(model,test,type = "response")
  
  results = ifelse(test = results_pr > 0.5,yes = 1,no = 0)
  
  answers = test$is_pass
  
  mis_classification = mean(answers!=results)
  
  acc[i] = 1 - mis_classification
  
  #Confusion Matrix
  
  con_mat = table(data = results,reference = answers)
  
  fp_rate[i] = con_mat[2]/(nrow(train_data_1)-smp_size)
  
  fn_rate[i] = con_mat[3]/(nrow(train_data_1)-smp_size)
  
  p_bar$step()
  
}

# Now we will load the test_data into the environment

test_data  = read.csv(file = "C:/Users/HP/Desktop/Dataquest Hkton/test_2nAIblo.csv")

#dummies_1 = model.matrix(object = ~program_type+test_type+difficulty_level
#  +gender+education+is_handicapped,data = test_data)

test_data_1 = test_data

test_data_1$age = impute(x = test_data_1$age,fun = median)

test_data_1$trainee_engagement_rating = impute(
  x = test_data_1$trainee_engagement_rating,fun = 1)

pred_test = predict(object = model,test_data_1[,-c(1,2)],type = "response")

pred_test = ifelse(test = pred_test>0.5,yes = 1,no = 0)

sol_n = data.frame(test_data_1$id,pred_test)

names(sol_n)[c(1,2)] = c("id","is_pass")

# Now we will save the sol_n dataframe into a csv format

save_result = write.csv(x = sol_n,file = "results.csv")

