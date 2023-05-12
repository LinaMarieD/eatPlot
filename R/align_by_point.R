string = "2.1"
x = 1
y = 1

build_alignement <- function(string, x, y){

  pos_point <- unlist(gregexpr('\\.', string))[1]

  before_point <- substr(string, 1, pos_point)

  strsplit(string, split = "\\.")

  c(
    ggtext::geom_richtext(
      data = ,
      ggplot2::aes(
        x = x,
        y = y,
        label = before_point
      )
    )
  )
}



ggplot2::ggplot() +
  build_alignement(string, x, y)
