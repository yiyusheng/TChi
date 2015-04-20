fun.test <- function(a, b, method = "add"){
  if(method == "add") { ## 如果if或者for/while；
    res <- a + b       ## 等后面的语句只有一行，则无需使用花括号。
  }
  if(method == "substract"){
    res <- a - b
  }
  return(res)           ## 返回值
}
### 检验结果
fun.test(a = 10, b = 8, method = "add")
fun.test(a = 10, b = 8, method = "substract")
