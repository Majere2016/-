library(scorecard)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
library(Formula)
library(smbinning)
library(zoo)

#输入数据
yonghupingfenka <- read.delim("~/Desktop/marryU/20220930/pingfenka_male.txt")
yonghupingfenka <- read.delim("~/Desktop/marryU/20220930/pinfenkafemale.txt")

#去掉uid和异常值
#为了区分加大，空值全部赋值-999
df<- yonghupingfenka[, -1]
df$education_flag<-as.numeric(df$education_flag)
df[is.na(df)] <- -999
df$education_flag<-as.numeric(df$education_flag)

#写一个函数来去掉空值

myfill <- function(df){
  df2 <- df %>%
    mutate(across(where(is.character),function(x){ replace(x,is.na(x),-999)})) %>%
    mutate(across(where(is.numeric),function(x) {replace(x,is.na(x),-999)}))
  return(df2)
}
df<-myfill(df)
#df$education_flag<-as.numeric(df$education_flag)
sqldf("select * from df where education_flag is null")
#筛选数据，要求iv大于0.02，并且缺失值不能高于0.5，且分布叫位均匀
#var_filter(dt, y, x = NULL, iv_limit = 0.02, missing_limit = 0.95,identical_limit = 0.95, var_rm = NULL, var_kp = NULL,return_rm_reason = FALSE, positive = "bad|1")
dt_f <- var_filter(df, "target")
print(dim(dt_f))

dt_list <- split_df(dt_f, "target")  
class(dt_list)
label_list <- lapply(dt_list, function(x) x$target)

#进行分箱，决策树和卡方都来一遍
#woebin(dt, y, x = NULL, var_skip = NULL, breaks_list = NULL,special_values = NULL, stop_limit = 0.1, count_distr_limit = 0.05,bin_num_limit = 8, positive = "bad|1", no_cores = NULL,print_step = 0L, method = "tree", save_breaks_list = NULL,ignore_const_cols = TRUE, ignore_datetime_cols = TRUE,check_cate_num = TRUE, replace_blank_inf = TRUE, ...)
#bins <- woebin(dt_list$train, "target")
#决策树分箱
bins2_chi = woebin(dt_list$train,y = "target", method = "chimerge", stop_limit = 0.5)
bins2_tree = woebin(dt_list$train,y = "target", , method = "tree")
dt_woe_list <- lapply(dt_list, function(x) woebin_ply(x, bins2_tree))
bins2_woe<-data.table::rbindlist(bins2_tree)

#建立逻辑回归模型，并且逐步回归法迭代
m1 <- glm(target ~ ., family = binomial(), data = dt_woe_list$train)
m_step <- step(m1, direction="both", trace=FALSE)
m2 <- eval(m_step$call)


#检测模型预测情况
pred_list <- lapply(dt_woe_list, function(x) predict(m2, type = 'response', x))
#利用模型建立评分卡
card <- scorecard(bins2_tree, m2)
score_list <- lapply(dt_list, function(x) scorecard_ply(x, card)) 
score_list$train


info_value = iv(df,y="target")

dt_sel = var_filter(df,y = 'target',iv_limit = 0.1,missing_limit = 0.75)

#建立全部评分卡
dt_woe = woebin_ply(df,bins = bins2_tree)
bins_df = data.table::rbindlist(dt_woe)

#对所有用户打分，基础分600，权重50
score <- scorecard(bins2_tree,model =m2 ,points0 = 600,odds0 = 50)
bins_germ_woe = data.table::rbindlist(score,fill = TRUE)
df_score<-scorecard_ply(dt = df,card = score)

hist(df_score)


score$basepoints
score$age_flag
#检测模型预测情况AUC是否高于0.85，KS是否高于0.5
perf_eva(pred = pred_list$train, label=dt_list$train$target,
         title = 'train')

#输出PSI情况，检查模型稳定性
psi <- perf_psi(score = score_list, label = label_list)
gains_table <- gains_table(score = score_list, label = label_list)

# 写出建模数据集x(bins_germ_woe,file = '/Users/mazhecheng/Desktop/marryU/20220930/bins_germ_woe_male.xlsx' ,colNames=TRUE)
write.xlsx(bins_germ_woe,file = '/Users/mazhecheng/Desktop/marryU/20220930/bins_germ_woe_female.xlsx' ,colNames=TRUE)

