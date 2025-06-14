
dataset=read.csv("AssessmentSet1.csv")

# 1. Association between shopping frequency and gender
shoppingGender.data= table(dataset$Gender,dataset$ShoppingFrequency)
shoppingGender.data

## Plotting the data
barplot(shoppingGender.data, beside=TRUE, col=c("light green", "light blue","red"),
        legend=c("Female", "Male","Other"),
        main="Association between shopping frequency and gender")

## testing
test=chisq.test(shoppingGender.data)
test$expected
chisq.test(shoppingGender.data, simulate.p.value = TRUE, B=1000)

# 2. The mean age of customers who shop at Hornsby is greater or lower than who shop at Bondi
age.horn=subset(dataset, StoreLocation=="Hornsby", Age, drop=TRUE)
age.horn
age.bon=subset(dataset, StoreLocation=="Bondi", Age, drop=TRUE)
age.bon

## Plotting the data
hist(age.horn,xlab="Age",main = "Customer at Hornsby", col="light blue")
hist(age.bon,xlab="Age",main = "Customer at Bondi",col="light green")

## testing
t.test(age.horn,age.bon,alternative="greater",var.equal = TRUE)


# 3. Difference in the mean satisfaction score across different genders
satisfac=table(dataset$Gender, dataset$SatisfactionScore)
satisfac

## Potting 
barplot(satisfac, beside=TRUE,
        legend=c("Female","Male","Other"))

## testing
table(dataset$Gender)
oneway.test(SatisfactionScore~Gender, data=dataset, var.equal = TRUE)

## Post-hoc test
diff.aov=aov(SatisfactionScore~Gender,data=dataset)
TukeyHSD(diff.aov)

#  4. Predict purchase amount
slope.perm=replicate(1000,{
  age.perm=sample(dataset$Age)
  perm=lm(PurchaseAmount~age.perm,data=dataset)
  coef(perm)[2]
})

act.predict=lm(PurchaseAmount~Age,data=dataset)
slope=coef(act.predict)[2]
mean(slope.perm>abs(slope))+mean(slope.perm<-abs(slope))

## Testing
act.predict=lm(PurchaseAmount~Age,data=dataset)
summary(act.predict)

## Transformation
trans=lm(log(PurchaseAmount)~Age,data=dataset)
summary(trans)

## Equation
y = a+bx
Purchases amount = 4.179577 + (0.012821*Age)

## Plotting
plot(trans)

## Predict y using the model
new.data=predict(trans,newdata =data.frame(Age=33))
exp(new.data) # because it is in the form of logarithm (log(y)), so we need to find the y