albers = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=110 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
cn_border = geocn::load_cn_border()
main_border = geocn::load_cn_landcoast()
tenline = geocn::load_cn_tenline()
province = geocn::load_cn_province()
city = geocn::load_cn_city() 

policy = haven::read_dta('./policy.dta')

city = city |> 
  dplyr::left_join(policy, by = dplyr::join_by(市代码 == city_code)) |> 
  dplyr::select(prov,city,first_policy_year) |> 
  dplyr::mutate(policy = dplyr::case_match(
    first_policy_year,
    2014 ~ "2014 cohort",
    2015 ~ "2015 cohort",
    2016 ~ "2016 cohort"))

hyp = terra::rast('hyp.tif')

library(tmap)

tm_shape(main_border,
         crs = sf::st_crs(albers)) +
  tm_lines(lwd = 0.01) +
  tm_shape(hyp) +
  tm_rgb() +
  tm_shape(province) +
  tm_fill(col = 'white',fill_alpha = .5) +
  tm_borders(col = 'grey40', lwd = 0.25) +
  tm_shape(city) +
  tm_polygons(fill = 'policy',
              fill_alpha = .75, lwd = 0.05,
              col_alpha = .45, col = NA,
              fill.scale = tm_scale_categorical(
                values = c('#84b7ee','#ffa95b','#96cfa6'),
                value.na = "transparent"
              ),
              fill.legend = tm_legend(title = "NTU Policy Year")) +
  tm_shape(cn_border) +
  tm_lines(col ='#9d98b7', lwd = 2.5) +
  tm_scalebar(position = c(0.05,0.01)) +
  tm_compass(position = c(0.05,0.9),
             just = 'center', size = 1.5,
             text.size = .65, show.labels = 1) +
  tm_layout(legend.position = c(0.045,0.075),
            compass.type = "arrow",
            text.fontfamily = "serif") -> cn_mainplot

tm_shape(tenline,
         crs = sf::st_crs(albers)) +
  tm_lines(col=NA,lwd=0.01) +
  tm_shape(hyp) +
  tm_rgb() +
  tm_shape(province) +
  tm_fill(col = 'white',fill_alpha = .5) +
  tm_borders(col = 'grey40', lwd = 0.05) +
  tm_shape(city) +
  tm_polygons(col = 'policy', alpha = .75,lwd = 0.05,
              border.alpha = .45,border.col = NA,
              palette = c('#84b7ee','#ffa95b','#96cfa6'),
              title = '',legend.show = F) +
  tm_shape(cn_border) +
  tm_lines(col='#9d98b7',lwd = 2.5) -> cn_miniplot

cowplot::ggdraw() +
  cowplot::draw_plot(tmap::tmap_grob(cn_mainplot)) +
  cowplot::draw_plot(tmap::tmap_grob(cn_miniplot),
                     halign = 0.5,valign = 0.5,
                     height = 0.2,
                     x = 0.430,
                     y = 0.018) -> cn_plot

ggplot2::ggsave('./fig.pdf',device = cairo_pdf,
                plot = cn_plot, width = 7.25, height = 6)
