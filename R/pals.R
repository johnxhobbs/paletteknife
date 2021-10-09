
#' Helper to get a set by name - will search all included lists ("pals."),
#' then built-in, or just returns itself if already a vector
#' @param set Character string of single palette name or a vector of colours
#' @param default What to use if an empty string is passed (the default case)
#' @import graphics
#' @import grDevices
get_set = function(set = '', default = 'turbo' ){
  if(set[1]=='')
    set = default

  if(length(set)==1) # assume you've given a name of a set
    set_palette = c(pals.misc, pals.rcolorbrewer, pals.viridis)[[set]]

  if(length(set)==1 & is.null(set_palette))  # try the built-in palette names
    set_palette = palette.colors(palette = set)

  if(length(set) > 1) # you've given your own palette vector
    set_palette = set

  return(set_palette)
}

#' Built-in Paletteknife Palettes
#'
#' Plot a list of palettes for comparison
#'
#' @examples
#' pals_display(c(list(rainbow = rainbow(10), default = palette()),
#'                pals.misc, pals.rcolorbrewer[c('Paired','Set1','Set2')] ))
#'
#' pals_display(list(rainbow = rainbow(45)[30:1], turbo = pals.viridis$turbo ))
#'
#' pals_display(lapply(setNames(palette.pals(),palette.pals()), palette.colors, n=NULL))
#'
#' # Bit of fun ordering a list of palettes (MUST be same palette size)
#' mat_cols = do.call(rbind, lapply(pals.rcolorbrewer[9:26],
#'                           function(hex) as.vector(rgb2hsv(col2rgb(hex)))))
#' pals_display(pals.rcolorbrewer[9:26][hclust(dist(mat_cols))$order])
#'
#' @param pals Named list of palettes (colour vectors)
#'
#' @import graphics
#' @import grDevices
#' @export
pals_display = function(pals = c(pals.misc,pals.rcolorbrewer,pals.viridis)){
  pal_names = names(pals)
  if(is.null(pal_names)) stop('Must be a NAMED list, e.g. "pals = list(myrainbow = rainbow(10), default = palette() )"')

  pal_lengths = unlist(lapply(pals,length))

  x_coords = rep(seq_along(pals), pal_lengths)
  y_coords = unlist(lapply(pals, function(x) rev(seq(from = 1, to = 0, length.out = length(x)+1 )[-1]) ))
  colours = unlist(pals)

  plot(x_coords, y_coords, xlim=c(1,1+length(pals)), ylim=c(0,1), col = colours, pch=15, xaxs='i', yaxs='i', type='n', xaxt='n', yaxt='n', ylab='', xlab='')
  axis(side = 1, at = +0.5 + 1:length(pal_names), labels = pal_names, las = 2, cex.axis = 0.8 )
  axis(side = 3, at = +0.5 + 1:length(pal_names), labels = pal_lengths, cex.axis = 0.8 )
  rect(xleft = x_coords, xright = x_coords+1, ybottom = y_coords, ytop = y_coords+1, col = colours, border = NA)
}


### BELOW are all imported colour palettes
# Use "pals.source" naming - these are imported into global NAMESPACE so keep them easy to find

#' @rdname autocol
#' @export
pals.misc = list(
  # Sasha Trubetskoy
  # List of 20 Simple, Distinct Colors, 2017
  sasha = c('#E6194B','#3CB44B','#FFE119','#0082C8','#F58230','#911EB4','#46F0F0','#F032E6','#D2F53C','#FABED4','#008080','#DCBEFF','#AA6E28','#FFFAC8','#800000','#AAFFC3','#808000','#FFD7B4','#000080','#808080','#000000')
)

#' @rdname autocol
#' @export
pals.viridis = list(
  turbo  =  c('#30123BFF','#3A2C78FF','#4145AAFF','#455DD1FF','#4773EBFF','#458AFCFF','#3B9FFDFF','#2DB6F1FF','#1FCADCFF','#18DBC5FF','#1FE9AFFF','#34F395FF','#54FA78FF','#76FE5BFF','#96FE44FF','#AFFA37FF','#C5F034FF','#DAE336FF','#EBD239FF','#F7C13AFF','#FDAC34FF','#FE942AFF','#F97A1EFF','#F25F14FF','#E7490CFF','#D83706FF','#C62703FF','#B11901FF','#970E01FF','#7A0403FF'),
  cividis = c('#00204DFF','#00275CFF','#002C6BFF','#00326FFF','#12386DFF','#263F6CFF','#34456BFF','#3F4C6BFF','#48526BFF','#51586CFF','#595F6DFF','#62656FFF','#6A6C71FF','#717174FF','#787877FF','#817F79FF','#898679FF','#918C78FF','#9A9377FF','#A39A76FF','#ADA274FF','#B6A971FF','#BFB06EFF','#C8B86AFF','#D2C066FF','#DCC860FF','#E6D159FF','#F0D852FF','#FAE149FF','#FFEA46FF'),
  viridis = c('#440154FF','#470E61FF','#481B6DFF','#482576FF','#46307EFF','#443B84FF','#404688FF','#3C508BFF','#38598CFF','#33628DFF','#2F6B8EFF','#2C738EFF','#287C8EFF','#25838EFF','#228C8DFF','#1F948CFF','#1E9D89FF','#20A486FF','#26AD81FF','#31B57BFF','#3FBC73FF','#4FC46AFF','#61CB5FFF','#75D054FF','#8BD646FF','#A2DA37FF','#B9DE28FF','#D1E11CFF','#E8E419FF','#FDE725FF'),
  plasma =  c('#0D0887FF','#240691FF','#340498FF','#43039EFF','#5102A3FF','#6001A6FF','#6E00A8FF','#7B02A8FF','#8707A6FF','#9410A2FF','#A01A9CFF','#AB2494FF','#B52F8CFF','#BE3885FF','#C7427CFF','#CF4C74FF','#D7566CFF','#DE6065FF','#E56A5DFF','#EB7556FF','#F0804EFF','#F58C46FF','#F9973EFF','#FCA338FF','#FDB130FF','#FEBE2AFF','#FCCC25FF','#FADB24FF','#F5E926FF','#F0F921FF'),
  mako  =   c('#0B0405FF','#150A12FF','#1E111EFF','#26172CFF','#2E1E39FF','#342548FF','#3A2C58FF','#3E3367FF','#403A77FF','#414285FF','#3F4C92FF','#3B5799FF','#38629DFF','#366B9FFF','#3575A1FF','#347FA4FF','#3489A6FF','#3492A8FF','#359DAAFF','#37A6ACFF','#3CB1ADFF','#43BAADFF','#4CC3ADFF','#5CCDADFF','#73D4ADFF','#8DDBB3FF','#A5E1BCFF','#BAE6C7FF','#CCEDD6FF','#DEF5E5FF'),
  magma  =  c('#000004FF','#040414FF','#0C0925FF','#150E39FF','#21114EFF','#2F1163FF','#3F0F72FF','#4D117BFF','#5B167EFF','#681C81FF','#762181FF','#842681FF','#922B80FF','#A02F7FFF','#AE347BFF','#BD3977FF','#CC3F71FF','#D8456CFF','#E44F64FF','#EE5B5EFF','#F56B5CFF','#F97A5DFF','#FC8B62FF','#FD9B6BFF','#FEAC76FF','#FEBD82FF','#FECD90FF','#FDDD9FFF','#FCECAEFF','#FCFDBFFF'),
  inferno = c('#000004FF','#040313FF','#0D0828FF','#180C3DFF','#260C51FF','#360961FF','#450A69FF','#530F6DFF','#61136EFF','#6F196EFF','#7D1E6DFF','#8C2369FF','#992766FF','#A72D60FF','#B43359FF','#C13A50FF','#CD4347FF','#D84D3EFF','#E25834FF','#EB6429FF','#F1731DFF','#F78112FF','#FA9107FF','#FCA108FF','#FCB216FF','#FAC42AFF','#F6D442FF','#F3E55FFF','#F2F483FF','#FCFFA4FF'),
  rocket =  c('#03051AFF','#0E0B22FF','#1B112BFF','#271534FF','#34193DFF','#421B45FF','#501D4CFF','#5D1F52FF','#6C1F56FF','#7A1F59FF','#891E5BFF','#981B5BFF','#A7185AFF','#B51657FF','#C41753FF','#D11F4CFF','#DC2A46FF','#E43841FF','#EB493EFF','#EF5A41FF','#F26B49FF','#F47B55FF','#F58A62FF','#F59A70FF','#F6A880FF','#F6B691FF','#F7C3A3FF','#F7D0B6FF','#F9DDC9FF','#FAEBDDFF')
)

#' @rdname autocol
#' @export
pals.rcolorbrewer = list(
  Accent = c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666'),
  Dark2 = c('#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666'),
  Paired = c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928'),
  Pastel1 = c('#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6','#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2'),
  Pastel2 = c('#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC','#CCCCCC'),
  Set1 = c('#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999'),
  Set2 = c('#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3'),
  Set3 = c('#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F'),

  Greys = c('#FFFFFF','#F0F0F0','#D9D9D9','#BDBDBD','#969696','#737373','#525252','#252525','#000000'),
  Reds = c('#FFF5F0','#FEE0D2','#FCBBA1','#FC9272','#FB6A4A','#EF3B2C','#CB181D','#A50F15','#67000D'),
  YlOrRd = c('#FFFFCC','#FFEDA0','#FED976','#FEB24C','#FD8D3C','#FC4E2A','#E31A1C','#BD0026','#800026'),
  OrRd = c('#FFF7EC','#FEE8C8','#FDD49E','#FDBB84','#FC8D59','#EF6548','#D7301F','#B30000','#7F0000'),
  Oranges = c('#FFF5EB','#FEE6CE','#FDD0A2','#FDAE6B','#FD8D3C','#F16913','#D94801','#A63603','#7F2704'),
  YlOrBr = c('#FFFFE5','#FFF7BC','#FEE391','#FEC44F','#FE9929','#EC7014','#CC4C02','#993404','#662506'),
  PuBu = c('#FFF7FB','#ECE7F2','#D0D1E6','#A6BDDB','#74A9CF','#3690C0','#0570B0','#045A8D','#023858'),
  PuBuGn = c('#FFF7FB','#ECE2F0','#D0D1E6','#A6BDDB','#67A9CF','#3690C0','#02818A','#016C59','#014636'),
  Blues = c('#F7FBFF','#DEEBF7','#C6DBEF','#9ECAE1','#6BAED6','#4292C6','#2171B5','#08519C','#08306B'),
  BuGn = c('#F7FCFD','#E5F5F9','#CCECE6','#99D8C9','#66C2A4','#41AE76','#238B45','#006D2C','#00441B'),
  Greens = c('#F7FCF5','#E5F5E0','#C7E9C0','#A1D99B','#74C476','#41AB5D','#238B45','#006D2C','#00441B'),
  YlGn = c('#FFFFE5','#F7FCB9','#D9F0A3','#ADDD8E','#78C679','#41AB5D','#238443','#006837','#004529'),
  GnBu = c('#F7FCF0','#E0F3DB','#CCEBC5','#A8DDB5','#7BCCC4','#4EB3D3','#2B8CBE','#0868AC','#084081'),
  YlGnBu = c('#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58'),
  RdPu = c('#FFF7F3','#FDE0DD','#FCC5C0','#FA9FB5','#F768A1','#DD3497','#AE017E','#7A0177','#49006A'),
  PuRd = c('#F7F4F9','#E7E1EF','#D4B9DA','#C994C7','#DF65B0','#E7298A','#CE1256','#980043','#67001F'),
  BuPu = c('#F7FCFD','#E0ECF4','#BFD3E6','#9EBCDA','#8C96C6','#8C6BB1','#88419D','#810F7C','#4D004B'),
  Purples = c('#FCFBFD','#EFEDF5','#DADAEB','#BCBDDC','#9E9AC8','#807DBA','#6A51A3','#54278F','#3F007D'),

  BrBG = c('#543005','#8C510A','#BF812D','#DFC27D','#F6E8C3','#F5F5F5','#C7EAE5','#80CDC1','#35978F','#01665E','#003C30'),
  PuOr = c('#7F3B08','#B35806','#E08214','#FDB863','#FEE0B6','#F7F7F7','#D8DAEB','#B2ABD2','#8073AC','#542788','#2D004B'),
  PiYG = c('#8E0152','#C51B7D','#DE77AE','#F1B6DA','#FDE0EF','#F7F7F7','#E6F5D0','#B8E186','#7FBC41','#4D9221','#276419'),
  PRGn = c('#40004B','#762A83','#9970AB','#C2A5CF','#E7D4E8','#F7F7F7','#D9F0D3','#A6DBA0','#5AAE61','#1B7837','#00441B'),
  RdBu = c('#67001F','#B2182B','#D6604D','#F4A582','#FDDBC7','#F7F7F7','#D1E5F0','#92C5DE','#4393C3','#2166AC','#053061'),
  RdGy = c('#67001F','#B2182B','#D6604D','#F4A582','#FDDBC7','#FFFFFF','#E0E0E0','#BABABA','#878787','#4D4D4D','#1A1A1A'),
  RdYlBu = c('#A50026','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFBF','#E0F3F8','#ABD9E9','#74ADD1','#4575B4','#313695'),
  RdYlGn = c('#A50026','#D73027','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#D9EF8B','#A6D96A','#66BD63','#1A9850','#006837'),
  Spectral = c('#9E0142','#D53E4F','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#E6F598','#ABDDA4','#66C2A5','#3288BD','#5E4FA2')
)

