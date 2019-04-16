
# загрузка пакетов
library('R.utils')               # gunzip() для распаковки архивов 
library('sp')                    # функция spplot()
library('ggplot2')               # функция ggplot()
library('RColorBrewer')          # цветовые палитры
require('rgdal')                 # функция readOGR()
library('broom')                 # функция tidy()
require('dplyr')                 # функция join()
library('scales')                # функция pretty_breaks()
library('mapproj')
library('gpclib')
library('maptools')

# Численность персонала, занятого научными исследованиями и разработками за 2016 год ----
gpclibPermit()

ShapeFileURL <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_RUS_shp.zip"
if (!file.exists('./data')) dir.create('./data')
if (!file.exists('./data/gadm36_RUS_shp.zip')) {
    download.file(ShapeFileURL, destfile = './data/gadm36_RUS_shp.zip')
}
# распаковать архив
unzip('./data/gadm36_RUS_shp.zip', exdir = './data/gadm36_RUS_shp')
# посмотреть список файлов распакованного архива
dir('./data/gadm36_RUS_shp')

Regions1 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_1.shp")

# делаем фактор из имён областей (т.е. нумеруем их)
Regions1@data$NAME_1 <- as.factor(Regions1@data$NAME_1)
Regions1@data$NAME_1

# загружаем статистику с показателями по регионам
stat.Regions <- read.csv2('science_2016.csv', stringsAsFactors = F)
stat.Regions$count_2016 <- as.numeric(stat.Regions$count_2016)

# вносим данные в файл карты
Regions1@data <- merge(Regions1@data, stat.Regions,
                       by.x = 'NAME_1', by.y = 'Region')
    
# задаём палитру
mypalette <- colorRampPalette(c('whitesmoke', 'coral3'))


spplot(Regions1, 'count_2016', main = 'Численность персонала',
       col.regions = mypalette(10), # цветовая шкала
       # (10 градаций)
       col = 'coral4', # цвет контурных линий
       par.settings = list(axis.line = list(col = NA)) # без
       # осей
)


# Бюджет Хакасии за 2016 год ----
gpclibPermit()

stat.Regions <- read.csv2('Khakass_budget.csv', stringsAsFactors = F)
stat.Regions$budget <- as.numeric(stat.Regions$budget)

Regions <- readOGR(dsn = './data/gadm36_RUS_shp', # папка
                   layer = 'gadm36_RUS_2') # уровень 
Regions@data$id <- Regions@data$NAME_2
Regions <- Regions[grepl('^RU.KK.', Regions$HASC_2), ]
Regions.points <- fortify(Regions, region = 'id')
Regions.df <- merge(Regions.points, Regions@data, by = 'id')
stat.Regions$id <- stat.Regions$District
Regions.df <- merge(Regions.df,
                   stat.Regions[, c('id',
                                    'budget')],
                   by = 'id')

centroids.df <- as.data.frame(coordinates(Regions))
centroids.df$id <- Regions@data$id
colnames(centroids.df) <- c('long', 'lat', 'id')


gp <- ggplot() +
  geom_polygon(data = Regions.df,
               aes(long, lat, group = group,
                   fill = budget)) +
  geom_path(data = Regions.df,
            aes(long, lat, group = group),
            color = 'coral4') +
  coord_map(projection = 'gilbert', orientation = c(90, 0, 100)) +
  scale_fill_distiller(palette = 'OrRd',
                       direction = 1,
                       breaks = pretty_breaks(n = 5)) +
  labs(x = 'Долгота', y = 'Широта',
       title = "Бюджет Хакасии") +
  geom_text(data = centroids.df,
            aes(long, lat, label = id))
gp

  