

check <- wcr %>% filter(Species == "Hood Canal summer-run chum salmon")
check

test <- wbd.hucs %>% filter(huc8 == 17110020)
test
plot(test)
ESU.spatial

scale_color_viridis_b(direction = -1)

?colorNumeric
