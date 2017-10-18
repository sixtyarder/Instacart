setwd("/Users/apple/Documents/instacart/data")
library(ggplot2)
library(gridExtra)
library(data.table)
library(cvTools)
library(sqldf)

aisles = read.csv("aisles.csv")
departments = read.csv("departments.csv")
products = read.csv("products.csv")

load(file = "order.prior.RData")
load(file = "order.train.RData")
load(file = "orders.RData")

# convert variable in order table
orders = orders[-which(orders$eval_set == "test"), ]
orders$order_id = as.integer(orders$order_id)
orders$user_id = as.integer(orders$user_id)
orders$eval_set = as.character(orders$eval_set)
orders$order_number = as.integer(orders$order_number)
orders$days_since_prior_order[which(is.na(orders$days_since_prior_order))] = 0
orders$days_since_prior_order = as.integer(orders$days_since_prior_order)
orders$order_dow = as.integer(orders$order_dow)
orders$order_hour_of_day = as.integer(orders$order_hour_of_day)

# convert variable in prior order table
order.prior$order_id = as.integer(order.prior$order_id)
order.prior$product_id = as.integer(order.prior$product_id)
order.prior$add_to_cart_order = as.integer(order.prior$add_to_cart_order)
order.prior$reordered = as.integer(order.prior$reordered)

# convert variable in train order table
order.train$order_id = as.integer(order.train$order_id)
order.train$product_id = as.integer(order.train$product_id)
order.train$add_to_cart_order = as.integer(order.train$add_to_cart_order)
order.train$reordered = as.integer(order.train$reordered)

# extract 1/300 of users from order table
cv.index = cvFolds(length(unique(orders$user_id)), K=10)
selected.user.id = sort(unique(orders$user_id))[cv.index$subsets[which(cv.index$which==10)]]
orders.cut = orders[which(orders$user_id %in% selected.user.id), ]

# extract orders from order prior table and order train table
cut.order = orders.cut$order_id[which(orders$eval_set == "prior")]
order.prior.cut = order.prior[which(order.prior$order_id %in% cut.order), ]
cut.order = orders.cut$order_id[which(orders$eval_set == "train")]
order.train.cut = order.train[which(order.prior$order_id %in% cut.order), ]
order.detail = rbind(order.prior.cut, order.train.cut)

# add order size variable to order detail table 
order.detail$order_size = rep(0, dim(order.detail)[1])
order.list = as.vector(unique(order.detail$order_id))
all.order.list = as.vector(order.detail$order_id)
for(i in 1:length(order.list)) {
  order.index = which(all.order.list == order.list[i])
  order.detail[order.index, "order_size"] = length(order.index)
}

# merge user order table with order detail table 
user_item_table = merge(orders.cut, order.detail, by.x = "order_id", by.y = "order_id", sort = TRUE)
user_item_table = user_item_table[order(user_item_table[, "user_id"], decreasing = TRUE), ]

# user related variable
user_item_table$user_order_span_mean = rep(0, dim(user_item_table)[1])
user_item_table$user_repeat_previous_ratio = rep(0, dim(user_item_table)[1])
user_item_table$user_reorder_count = rep(0, dim(user_item_table)[1])
user_item_table$user_average_order_size = rep(0, dim(user_item_table)[1])
user_item_table$user_total_order_span = rep(0, dim(user_item_table)[1])
user_item_table$user_max_item_buy = rep(0, dim(user_item_table)[1])

# user product interaction variable
user_item_table$up_total_order = rep(0, dim(user_item_table)[1])
user_item_table$up_days_since_last_order = rep(0, dim(user_item_table)[1])
user_item_table$up_streak_of_2 = rep(0, dim(user_item_table)[1])
user_item_table$up_position_cart = rep(0, dim(user_item_table)[1])
user_item_table$up_ratio_of_ordered = rep(0, dim(user_item_table)[1])
user_item_table$up_co_occur_count = rep(0, dim(user_item_table)[1])
user_item_table$up_last_order_minus_order_span = rep(0, dim(user_item_table)[1])
user_item_table$up_order_after_0_ratio = rep(0, dim(user_item_table)[1])
user_item_table$up_order_after_1_ratio = rep(0, dim(user_item_table)[1])
user_item_table$up_order_after_first_check = rep(0, dim(user_item_table)[1])
eval_set = user_item_table$eval_set
user_item_table$eval_set = NULL
user_item_mat = as.matrix(user_item_table)

# product related variable
product.list = sort(unique(user_item_table$product_id))
product.attr = data.frame(product_id = product.list,
                          product_ordered_time_cycle = rep(0, length(product.list)),
                          product_total_order_count = rep(0, length(product.list)),
                          product_total_order_span = rep(0, length(product.list)),
                          product_total_cooccur_count = rep(0, length(product.list)),
                          product_total_cart_pos = rep(0, length(product.list)),
                          product_buy_just_once_count = rep(0, length(product.list)),
                          product_streak_2_count = rep(0, length(product.list)),
                          product_all_01_count = rep(0, length(product.list)),
                          product_all_previous_0_count = rep(0, length(product.list)),
                          product_all_previous_1_count = rep(0, length(product.list)),
                          product_order_after_0 = rep(0, length(product.list)),
                          product_order_after_1 = rep(0, length(product.list)),
                          product_first_order_count = rep(0, length(product.list)),
                          product_order_after_first_order = rep(0, length(product.list)))
product_attr_mat = as.matrix(product.attr)

# user item table variable list
user.list = as.vector(sort(unique(user_item_mat[, "user_id"])))   #  unique user_id list
all.user.list = as.vector(user_item_mat[, "user_id"])             #  duplicate user_id list
all.product.list = as.vector(user_item_mat[, "product_id"])       #  duplicate product_id list

all.order.list = as.vector(user_item_mat[, "order_id"])           # duplicate order_id list
order.span.list = as.vector(user_item_mat[, "days_since_prior_order"])  # order span list
order.size.list = as.vector(user_item_mat[, "order_size"])        # order size list
product.list = as.vector(unique(order.detail[, "product_id"]))      # unique product list

for(i in 1:length(user.list)) {
  user.index = which(all.user.list == user.list[i])
  user_partial_mat = user_item_mat[user.index, ]     # user partial table
  if(length(user.index) == 1) {
    user_partial_mat = t(as.matrix(user_partial_mat))
  }
  user_item_mat[user.index, "user_order_span_mean"] = mean(order.span.list[user.index])       # order_span_mean
  user_item_mat[user.index, "user_average_order_size"] = mean(order.size.list[user.index])    # average order size
  
  user_duplicate_product_list = user_partial_mat[, "product_id"]
  user_product_total_count = length(user_duplicate_product_list)
  user_partial_product_list = sort(as.vector(unique(user_partial_mat[, "product_id"])))
  user.order.list = sort(unique(user_item_mat[user.index, "order_number"]))
  product_number = length(user_partial_product_list)
  order_count = length(user.order.list)
  max_order_span = max(user_partial_mat[, "days_since_prior_order"])
  
  user_order_span_list = rep(0, order_count)
  product_each_order_record = matrix(rep(0, product_number*(order_count+1)), product_number, order_count+1)
  for(m in 1:product_number) {
    product_each_order_record[m, 1] = user_partial_product_list[m]
  }
  
  user_item_mat[user.index, "user_reorder_count"] = sum(user_partial_mat[, "reordered"]) / order_count   # user total reordered count
  
  # go through every order of one user
  user.repeat.count = 0
  user.none.repeat.count = 0
  user.total.order.span = 0
  for(j in 1:length(user.order.list)) {
    user_with_order_index = which(user_item_mat[, "user_id"]==user.list[i] & user_item_mat[, "order_number"]==user.order.list[j])
    reordered.count = sum(user_item_mat[user_with_order_index, "reordered"])
    #user_item_table[user_with_order_index, "user_reordered_count"] = reordered.count   # user total reordered count
    if(reordered.count>0) {
      user.repeat.count = user.repeat.count + 1
    } else {
      user.none.repeat.count = user.none.repeat.count + 1
    }
    user.total.order.span = user.total.order.span + user_item_mat[, "days_since_prior_order"][user_with_order_index[1]]
    user_order_span_list[j] = user_item_mat[user_with_order_index[1], "days_since_prior_order"]
    # go through a single order list
    single_order_product_list = user_item_mat[user_with_order_index, "product_id"]
    single_order_product_list_length = length(single_order_product_list)
    
    # fill product record in each order
    for(j2 in 1:product_number) {
      if(user_partial_product_list[j2] %in% single_order_product_list) {
        product_each_order_record[j2, j+1] = 1
      } else {
        product_each_order_record[j2, j+1] = 0
      }
    }
    
    for(j1 in 1:single_order_product_list_length) {
      product_index = which(product_attr_mat[, "product_id"]==single_order_product_list[j1])
      # product total co-occur count
      product_attr_mat[product_index, "product_total_cooccur_count"] = product_attr_mat[product_index, "product_total_cooccur_count"] +
                                                                   single_order_product_list_length - 1
      product_attr_mat[product_index, "product_total_cart_pos"] = product_attr_mat[product_index, "product_total_cart_pos"] + 
                                                              user_item_mat[user_with_order_index[j1], "add_to_cart_order"]
    }
  }
  user.repeat.ratio = user.repeat.count / length(user.order.list) 
  user_item_mat[user.index, "user_repeat_previous_ratio"] = user.repeat.ratio   # user repeat previous ratio
  user_item_mat[user.index, "user_total_order_span"] = user.total.order.span   # user total order span
  
  # product count of order streak of 2
  for(q in 1:product_number) {
    product_id_mat = product_each_order_record[q, 1]
    order_after_0_cnt = 0
    order_after_1_cnt = 0
    if_order_after_first = 0

    v = rep(0, order_count-1)
    for(q1 in 1:order_count-1) {
      v[q1] = product_each_order_record[q, q1+1] + product_each_order_record[q, q1+2]
      if(product_each_order_record[q, q1+1] == 0 & product_each_order_record[q, q1+2] == 1) {
        order_after_0_cnt = order_after_0_cnt + 1
      }
      if(product_each_order_record[q, q1+1] == 1 & product_each_order_record[q, q1+2] == 1) {
        order_after_1_cnt = order_after_1_cnt + 1
      }
    }
    cut_last_record = product_each_order_record[q, 2:order_count]
    record = product_each_order_record[q, 2:(order_count+1)]
    if(sum(record)>1) {
      if_order_after_first = 1
    }
    
    last_order_span = sum(user_order_span_list[last(which(record == 1)):order_count])
    # set user product interaction term
    user_product_index = which(all.user.list==user.list[i] & all.product.list==product_id_mat)
    user_item_mat[user_product_index, "up_days_since_last_order"] = last_order_span
    user_item_mat[user_product_index, "up_last_order_minus_order_span"] = last_order_span - max_order_span
    user_item_mat[user_product_index, "up_streak_of_2"] = length(which(v==2))
    user_item_mat[user_product_index, "up_position_cart"] = mean(user_item_table$add_to_cart_order[user_product_index])
    user_item_mat[user_product_index, "up_ratio_of_ordered"] = sum(record) / user_product_total_count
    user_item_mat[user_product_index, "up_co_occur_count"] = user_product_total_count - sum(record)
    user_item_mat[user_product_index, "up_order_after_0_ratio"] = order_after_0_cnt / length(which(cut_last_record == 0))
    user_item_mat[user_product_index, "up_order_after_1_ratio"] = order_after_1_cnt / length(which(cut_last_record == 1))
    user_item_mat[user_product_index, "up_order_after_first_check"] = if_order_after_first
    
    pid = which(product_attr_mat[, "product_id"]==product_id_mat)
    product_attr_mat[pid, "product_streak_2_count"] = product_attr_mat[pid, "product_streak_2_count"] + length(which(v==2))
    product_attr_mat[pid, "product_all_01_count"] = product_attr_mat[pid, "product_all_01_count"] + order_count
    product_attr_mat[pid, "product_all_previous_0_count"] = product_attr_mat[pid, "product_all_previous_0_count"] + 
                                                        length(which(cut_last_record == 0))
    product_attr_mat[pid, "product_all_previous_1_count"] = product_attr_mat[pid, "product_all_previous_1_count"] +
                                                        length(which(cut_last_record == 1))
    product_attr_mat[pid, "product_order_after_0"] = product_attr_mat[pid, "product_order_after_0"] + order_after_0_cnt
    product_attr_mat[pid, "product_order_after_1"] = product_attr_mat[pid, "product_order_after_1"] + order_after_1_cnt
    product_attr_mat[pid, "product_first_order_count"] = product_attr_mat[pid, "product_first_order_count"] + 1
    product_attr_mat[pid, "product_order_after_first_order"] = product_attr_mat[pid, "product_order_after_first_order"] + if_order_after_first
  }
  
  # list all product user bought
  product_freq_table = table(user_partial_mat[, "product_id"])       # product frequency of 
  user_item_mat[user.index, "user_max_item_buy"] = max(product_freq_table)
  for(k in 1:length(user_partial_product_list)) {
    product_id = user_partial_product_list[k]
    product_index = which(product_attr_mat[, "product_id"]==product_id)
    product_attr_mat[product_index, "product_total_order_count"] = 
      product_attr_mat[product_index, "product_total_order_count"] + product_freq_table[[k]] 
    product_attr_mat[product_index, "product_total_order_span"] = 
      product_attr_mat[product_index, "product_total_order_span"] + user.total.order.span 
    # user just buy once count
    if(product_freq_table[[k]] == 1) {
      product_attr_mat[product_index, "product_buy_just_once_count"] = product_attr_mat[product_index, "product_buy_just_once_count"] + 1
    }
    
    # set user product interaction term
    user_product_index = which(all.user.list==user.list[i] & all.product.list==product_id)
    user_item_mat[user_product_index, "up_total_order"] = product_freq_table[[k]]
  }
  
}

################## set product related feature ####################
product.attr = as.data.frame(product_attr_mat)
product.attr$product_ordered_time_cycle = product.attr$product_total_order_span / product.attr$product_total_order_count
product.attr$product_ratio_of_ordered = product.attr$product_total_order_count / product.attr$product_all_01_count
product.attr$product_co_occur_per_order = product.attr$product_total_cooccur_count / product.attr$product_total_order_count
product.attr$product_average_cart_pos = product.attr$product_total_cart_pos / product.attr$product_total_order_count
product.attr$product_order_ratio_after_0 = product.attr$product_order_after_0 / product.attr$product_all_previous_0_count
product.attr$product_order_ratio_after_1 = product.attr$product_order_after_1 / product.attr$product_all_previous_1_count
product.attr$product_order_ratio_after_first = product.attr$product_order_after_first_order / product.attr$product_total_order_count
product_table = product.attr[, c("product_id", "product_ordered_time_cycle", "product_total_order_count", "product_buy_just_once_count",
                                "product_streak_2_count", "product_ratio_of_ordered", "product_co_occur_per_order",
                                "product_average_cart_pos", "product_order_ratio_after_0", "product_order_ratio_after_1",
                                "product_order_ratio_after_first")]
product_table[which(is.nan(product_table$product_order_ratio_after_0)), "product_order_ratio_after_0"] = 0
product_table[which(is.nan(product_table$product_order_ratio_after_1)), "product_order_ratio_after_1"] = 0
save(product_table, file = "product_table.RData")

# assign reorder label for each user/product pair
# if user only has one order, assign -1 to all products
# assign -1 to all products in last order
user_item_table = as.data.frame(user_item_mat)
user_item_table[which(is.nan(user_item_table$up_order_after_0_ratio)), "up_order_after_0_ratio"] = 0
user_item_table[which(is.nan(user_item_table$up_order_after_1_ratio)), "up_order_after_1_ratio"] = 0

user_item_table$repeat_in_last_purchase = rep(0, dim(user_item_table)[1])     # add target column
dup_user_lst = as.vector(user_item_table$user_id)
dup_order_number_lst = as.vector(user_item_table$order_number)
user_lst = unique(user_item_table$user_id)

for(i in 1:length(user_lst)) {
  single_user_index = which(dup_user_lst == user_lst[i])
  single_user_table = user_item_table[single_user_index, ]
  order_number_list = unique(single_user_table$order_number)
  if(length(order_number_list) == 1) {
    user_item_table[single_user_index, "repeat_in_last_purchase"] = -1
  } else {
    last_order_product_list = single_user_table[which(single_user_table$order_number==max(order_number_list)), "product_id"]
    other_order_index = which(single_user_table$order_number != max(order_number_list))
    other_order_table = single_user_table[other_order_index, ]
    check_list = (sort(other_order_table$product_id) %in% sort(last_order_product_list))
    
    other_order_index_all = which(dup_user_lst==user_lst[i] & dup_order_number_lst != max(order_number_list))
    last_order_index_all = which(dup_user_lst==user_lst[i] & dup_order_number_lst == max(order_number_list))
    user_item_table[other_order_index_all, "repeat_in_last_purchase"] = sapply(check_list, function(x){if(x==FALSE) 0 else 1})
    user_item_table[last_order_index_all, "repeat_in_last_purchase"] = -1
  }
  
  if(i %% 730 == 0) cat("10 pct")
}

# delete last order data
save(user_item_table, file = "user_item_table.RData")
user_item_table = user_item_table[-which(user_item_table$repeat_in_last_purchase == -1), ]

# remove duplicated user/product pair
non_duplicated_user_item_table_index = c()
user_item_table_user_list = unique(user_item_table$user_id)
for(i in 1:length(user_item_table_user_list)) {
  single_user_index = which(user_item_table$user_id == user_item_table_user_list[i]) 
  single_user_table = user_item_table[single_user_index, ]
  single_user_prod_list = unique(single_user_table$product_id)
  
  for(j in 1:length(single_user_prod_list)) {
    user_prod_index = which(user_item_table$user_id == user_item_table_user_list[i] & user_item_table$product_id == single_user_prod_list[j])
    non_duplicated_user_item_table_index = c(non_duplicated_user_item_table_index, user_prod_index[1])
  }
}

# merge to form training data
user_product_table = merge(user_item_table[non_duplicated_user_item_table_index, ],
                           product_table, by.x = "product_id", by.y = "product_id", sort = TRUE) 
user_product_table = user_product_table[order(user_product_table[, "user_id"], decreasing = TRUE), ]
save(user_product_table, file = "user_product_table.RData")

# user order count plot, item purchase count plot
user_order_freq = table(orders$user_id)
user_order_count_list = rep(0, length(unique(orders$user_id)))
for(i in 1:length(unique(orders$user_id))) {
  user_order_count_list[i] = user_order_freq[[i]]
}
user_order_count_list = user_order_count_list[which(user_order_count_list<60)]

product_count_list = rep(0, length(unique(order.train$product_id)))
product_freq = table(order.train$product_id)
for(i in 1:length(unique(order.train$product_id))) {
  product_count_list[i] = product_freq[[i]]
}
product_count_list = product_count_list[which(product_count_list<80)]

#########################  model training #####################
load(file = "user_product_table.RData")
#user_product_table$product_id = as.factor(user_product_table$product_id)
#user_product_table$user_id = as.factor(user_product_table$user_id)
user_product_table$order_hour_of_day = as.factor(user_product_table$order_hour_of_day)
user_product_table$order_dow = as.factor(user_product_table$order_dow)
user_product_table$add_to_cart_order = as.ordered(user_product_table$add_to_cart_order)
user_product_table$reordered = as.factor(user_product_table$reordered)
user_product_table$order_number = as.ordered(user_product_table$order_number)

train_table = user_product_table
train_table$user_how_many_none = NULL
train_table$user_id = NULL
train_table$product_id = NULL
train_table$order_id = NULL

TREE_NUM = 2000
SHRINKAGE = 0.01
INTERACTION_NUM = 5
TERMINAL_NODE_NUM = 15

reorder_prediction_model = gbm(repeat_in_last_purchase ~ ., 
                                              data = train_table,
                                              distribution = "bernoulli",
                                              keep.data = FALSE,
                                              verbose = TRUE,
                                              n.trees = TREE_NUM,
                                              shrinkage = SHRINKAGE,
                                              interaction.depth = INTERACTION_NUM,
                                              n.minobsinnode = TERMINAL_NODE_NUM)

predicted_purchase_prob = predict(reorder_prediction_model, train_table, n.trees = TREE_NUM, type = "response") 
summary(reorder_prediction_model)
predicted_user_product_table = cbind(user_product_table[, c("user_id", "product_id", "repeat_in_last_purchase")], 
                                  predicted_purchase_prob)

##################### make prediction based on best F1-score ######################
final_compare_table = data.frame(predicted_purchase=c(), true_purchase=c()) 
user_purchase_prediction = list()

all_user_list = predicted_user_product_table$user_id
unique_user_list = unique(all_user_list)
for(i in 1:length(unique_user_list)) {

  first_user_index = which(all_user_list == unique_user_list[i])
  single_user_table = predicted_user_product_table[first_user_index, ]
  single_user_table = single_user_table[order(single_user_table$predicted_purchase_prob), ]
  predicted_purchase_list = single_user_table$predicted_purchase_prob

  SIMULATION_NUM = 50
  purchase_list_simulation = matrix(0, length(predicted_purchase_list), SIMULATION_NUM)
  for(j in 1:SIMULATION_NUM) {
    prob_compare_list = (predicted_purchase_list > runif(length(predicted_purchase_list)))
    simulated_purchase_list = sapply(prob_compare_list, function(x){if(x==FALSE) 0 else 1})
    purchase_list_simulation[, j] = simulated_purchase_list
  }

  # search for best purchase list which maximize F1 score
  previous_purchase_point = 1
  previous_f1_score = 0

  while(TRUE) {
    if(previous_purchase_point == length(predicted_purchase_list)) {
      break
    }
  
    user_purchase_list = rep(1, length(predicted_purchase_list))
    if(previous_purchase_point > 1) {
      user_purchase_list[1:(previous_purchase_point-1)] = 0
    }
    # compute average F1 score for all user purchase simulation 
    total_f1_score = 0
    for(k in 1:SIMULATION_NUM) {
      single_simulated_list = purchase_list_simulation[, k]
      true_positive = length(which(single_simulated_list==1 & user_purchase_list==1))
      false_positive = length(which(single_simulated_list==0 & user_purchase_list==1))
      false_negative = length(which(single_simulated_list==1 & user_purchase_list==0))
  
      b1 = true_positive + false_positive
      b2 = true_positive + false_negative
      if(true_positive != 0) {
        precision = true_positive / (true_positive + false_positive)
        recall = true_positive / (true_positive + false_negative)
        f1_score = 2*precision*recall / (precision + recall)
        total_f1_score = total_f1_score + f1_score
      }
    }
    average_f1_score = total_f1_score / SIMULATION_NUM

    if(average_f1_score < previous_f1_score) {
      break  
    }
    previous_f1_score = average_f1_score
    previous_purchase_point = previous_purchase_point + 1
  }

  predicted_buy_list = rep(0, length(predicted_purchase_list))
  predicted_buy_list[previous_purchase_point:length(predicted_purchase_list)] = 1
  user_compare_table = data.frame(predicted_purchase=predicted_buy_list, true_purchase=single_user_table$repeat_in_last_purchase)

  single_user_product_prediction_list = single_user_table$product_id[previous_purchase_point:length(predicted_purchase_list)]
  single_user_list = list(user_id=unique_user_list[i], product_purchase_list = single_user_product_prediction_list)

  final_compare_table = rbind(final_compare_table, user_compare_table)
  user_purchase_prediction[[i]] = single_user_list
  
}
