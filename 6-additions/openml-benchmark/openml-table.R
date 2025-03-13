
task_id <- c(3, 31, 37, 43, 49, 219, 3485, 3492, 3493, 3494, 3891, 
             3899, 3902, 3903, 3913, 3917, 3918, 3954, 9946, 9952, 
             9957, 9967, 9970, 9971, 9976, 9978, 9980, 9983, 100093, 
             10101, 14965, 34537, 34539)
length(task_id)

maj_cls_size <- c(1669, 700, 500, 2788, 626, 26075, 1976, 278, 395, 
                  288, 1763, 10437, 1280, 1403, 415, 1783, 1032, 12332, 
                  357, 3818, 699, 1268, 606, 416, 1300, 2374, 494, 
                  8257, 762, 570, 39922, 6157, 30872)
length(maj_cls_size)

min_cls_size <- c(1527, 300, 268, 1813, 332, 19237, 431, 278, 206, 
                  266, 1705, 5108, 178, 160, 107, 326, 77, 6688, 212, 
                  1586, 356, 673, 606, 167, 1300, 160, 46, 6723, 610, 
                  178, 5289, 4898, 1897)
length(min_cls_size)

no_of_feats <- c(31, 21, 9, 58, 10, 9, 300, 7, 7, 7, 971, 6, 38, 
                 38, 22, 22, 22, 12, 31, 6, 42, 34, 101, 11, 501, 
                 73, 21, 15, 5, 5, 17, 31, 10)
length(no_of_feats)

sample_size <- maj_cls_size + min_cls_size
sample_size

df <- data.frame(task_id = task_id, 
                 maj_cls_size = maj_cls_size, 
                 min_cls_size = min_cls_size, 
                 no_of_feats = no_of_feats, 
                 sample_size = sample_size)


df$maj_by_min <- df$maj_cls_size / df$min_cls_size
df$obs_per_feat <- df$sample_size / df$no_of_feats

summary(df$maj_cls_size)
