library(ggplot2)
library(gridExtra)
library(Hmisc)
library(extrafont)

library(Cairo)
#font_import()
loadfonts()


graph_w = 5
graph_h = 5

########### end of header #####################

# reward field
x1 = 0
x2 = 0
A1_ = function(x1, x2){
  H = 20
  c(H - x1 - x2, H - x1 - x2) / 50
}
x = data.frame(x1 = c(), x2 = c(), Ax1 = c(), Ax2 = c())
for (i in c(1:10)){
  for (j in c(1:10)){
    A1_x = A1_(i, j) 
    res = data.frame(x1 = i, x2 = j, Ax1 = A1_x[1], Ax2 = A1_x[2])    
    x = rbind(x, res)
  }
}

library(ggplot2)
ggplot(data=x, aes(x=x1, y=x2)) + 
  geom_segment(aes(xend=x1+Ax1, yend=x2+Ax2), arrow = arrow(length = unit(0.1,"cm"))) +
  scale_x_continuous(name = expression(O["2"] ( a.u))) + 
  scale_y_continuous(name = expression(O["1"] ( a.u))) +
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = "right", legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),      
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=20),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(1.5, 'lines')
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
ggsave(file="../graphs/reward_field_same_drive.pdf",  width=15 , height=15, units = "cm", device=cairo_pdf)

A2_ = function(x1, x2){
  H = 10
  c(H - x1, H - x2) / 20
}

x = data.frame(x1 = c(), x2 = c(), Ax1 = c(), Ax2 = c())
for (i in c(1:10)){
  for (j in c(1:10)){
    A1_x = A2_(i, j) 
    res = data.frame(x1 = i, x2 = j, Ax1 = A1_x[1], Ax2 = A1_x[2])    
    x = rbind(x, res)
  }
}

library(ggplot2)
ggplot(data=x, aes(x=x1, y=x2)) + 
  geom_segment(aes(xend=x1+Ax1, yend=x2+Ax2), arrow = arrow(length = unit(0.1,"cm"))) +
  scale_x_continuous(name = expression(O["2"] ( a.u))) + 
  scale_y_continuous(name = expression(O["1"] ( a.u))) +
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = "right", legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),      
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=20),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(1.5, 'lines')
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
ggsave(file="../graphs/reward_field_different_drive.pdf",  width=15 , height=15, units = "cm", device=cairo_pdf)


# effect of respone cost
T = 50
k = 1
l = 1
a = 1
b = 1
H = 8

b = seq(3, 7, 1) 
y = k * ((H * l - b * k) / (T * l  *l + 2 * a * k * k))

data = data.frame(x = b, y = 1 / y)
file_name = "force"
draw_graph(data, file_name, "cost (a.u)", "inter-response\n interval", graph_w = 6, graph_h = 4)

# effect of ratio requirement
T = 50
k = 1
l = 1
a = 0.3
b = 0.05
H = 100

k = seq(1, 100, 1) 
y = k * ((H * l - b * k) / (T * l  *l + 2 * a * k * k))

data = data.frame(x = k, y = y)
draw_graph(data, "ratio", "ratio requirement (k)")


# effect of drive
T = 50
k = 1
l = 1
a = 0.3
b = 0.05
H = 100

H = seq(10, 100, 1) 
y = k * ((H * l - b * k) / (T * l  *l + 2 * a * k * k))

data = data.frame(x = H, y = y)
draw_graph(data, "drive", "deprivation level (a.u)")

# reinforcer magnitude
T = 50
k = 1
l = 1
a = 0.1
b = 0.1
H = 100

l = seq(0, 1, .01) 
y = k * ((H * l - b * k) / (T * l  *l + 2 * a * k * k))

data = data.frame(x = l, y = y)
draw_graph(data, "reward_mag", "reward magnitude (a.u)")

# for drawing cost and reward field ###################################################
a = 0.02 * 16
b = 0
k = 4
dv = 0.01
v1 = seq(5, 1, -dv)
v2 = v1 * 0 + 3

t = seq(0, 60 - 60 / length(v1), 60 / length(v1))

#v = v * 0 + 10.5
cost = c()
H = 5
R1 = 0
R2 = 0
A = H
C1 = 0
C2 = 0
reward1 = c()
cost1 = c()
reward2 = c()
cost2 = c()
v1_type = c()
v2_type = c()
dt = diff(t)[1]
for (i in 1:length(v1)){
  R1 = R1 + A * v1[i] * dt / k
  kv1 = a * k ** 2 * v1[i] ** 2 / k ** 2 + k * b * v1[i] / k #devisions by k are because v's are in the response rates space not outcome space
  C1 = C1 + kv1 * dt
  
  R2 = R2 + A * v2[i] * dt / k
  kv2 = a * k ** 2 * v2[i] ** 2 / k ** 2 + k * b * v2[i] / k
  C2 = C2 + kv2 * dt
  
  reward1 = c(reward1, R1)
  cost1 = c(cost1, C1)  
  
  reward2 = c(reward2, R2)
  cost2 = c(cost2, C2)  
  
  v1_type = c(v1_type, "variable \n response rates")
  v2_type = c(v2_type, "constant \n response rates")
}

data = data.frame(t=t, cost = c(cost1, cost2), reward= c(reward1, reward2),
                  v = c(v1, v2),
                  v_type = c(v1_type, v2_type))

library(reshape)
data_m = melt(data, id = c("t", "v_type"))

data_m$variable = factor(data_m$variable, levels = c(levels(data_m$variable), "response rate (v)", "total reward", "total cost"))
data_m[data_m$variable == "v",]$variable = "response rate (v)"
data_m[data_m$variable == "reward",]$variable = "total reward"
data_m[data_m$variable == "cost",]$variable = "total cost"
#data_m$variable = factor(data_m$variable, rev(levels(data_m$variable)))

library(ggplot2)
ggplot(data_m, aes(x = t, y = value, colour=v_type)) + 
  geom_line(aes(linetype=v_type)) + 
  scale_x_continuous(name = "time (min)") + 
  scale_y_continuous(name = "") +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = "right", legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),      
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=10),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(1.5, 'lines')
  ) +
  facet_wrap(~ variable, scales="free_y") + 
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

ggsave(file="../graphs/cost_reward.pdf",  width=15 , height=5, units = "cm", device=cairo_pdf)

# effect of session duration ##########################################################
k = 15
l = 1.
a = 0.05
b = 0.1
H = 450

t = seq(0, 60, 1) 

#mean = 50
#sd = 20
#T = etruncnorm(a = t, b = Inf, mean = mean, sd = sd) / (1 - pnorm(t, mean = mean, sd = sd)) 

ks = 0.7
sigma = 60 * (1 - ks)

T = (ks * t + sigma) / (1 - ks) 

#T = 60 -t
y = c()
H_l = H
for (i in 1:length(t)){
  v = ((H_l * l - b * k) / (T[i] * l  *l + 2 * a * k * k))
  y = c(y, k * v)
  H_l = H_l  - l * v
}
y


data = data.frame(x = t, y = y, cond="decreasing reward\n and unknown\n session duration\n")

y = c()
H_l = H / 4
for (i in 1:length(t)){
  v = ((H_l * l - b * k) / (2 * a * k * k))
  y = c(y, k * v)
}
y

data = rbind(data, data.frame(x = t, y = y, cond="fixed reward\n(known or unknown\n session duration)\n"))

y = c()
H_l = H
Ti = 60
for (i in 1:length(t)){
  v = ((H_l * l - b * k) / (Ti * l  *l + 2 * a * k * k))
  y = c(y, k * v)
}
y

data = rbind(data, data.frame(x = t, y = y, cond="known session\n duration\n(fixed or\n decreasing reward)\n"))


ggplot(data, aes(x = x, y = y, color=cond)) + 
  geom_line(aes(linetype=cond)) + 
  scale_x_continuous(name = "time (min)", expand = c(0,0)) + 
  scale_y_continuous(name = "response rate", limits = c(0, 130), expand = c(0,0)) +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = "right", legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),      
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=10),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank()
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
ggsave(file="../graphs/duration_fixed_variable.pdf",  width=9.5 , height=5.5, units = "cm", device=cairo_pdf)

# for drawing distribution  ##########################################################
ks = 0.7
sigma = 60 * (1 - ks)
t0 = 0

data= data.frame(t=c(), ft=c(), t0=c())

for(t0 in c(0, 15, 30)){
  sigma_t = ks * t0 + sigma
  t = seq(t0, 200, 1)
  
  #medT = t0 * (2 ^ ks - 1) + sigma * (2 ^ ks - 1) / ks
  ET = (sigma_t) / (1 - ks) + t0
  
  ft = 1. / sigma_t * (1 + ks * (t - t0) / sigma_t) ^ -(1/ks + 1)
  
  data = rbind(data, data.frame(t =t, ft = ft, t0= t0, ET=ET))
  
}

data$tp0 = paste(expression("t'="), data$t0)
ggplot(data, aes(x = t, y = ft, fill="red")) + 
  geom_area() + 
  scale_x_continuous(name = "T(s)", expand = c(0,0), limits = c(0, 200)) + 
  scale_y_continuous(name = "p(T)", expand = c(0,0)) +
  scale_fill_brewer(palette="Set1") + 
  geom_vline(aes(xintercept = ET), linetype = "longdash") +
  geom_vline(aes(xintercept = t0), size = 1) +
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = "none", legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),    
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=10),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  )   +facet_wrap(~ tp0, nrow = 1) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
ggsave(file="../graphs/pT.pdf",  width=15, height=4, units = "cm", device=cairo_pdf)


# effect of independent and dependent costs  ##########################################################
k = 1
l = 2
a = 1
b = NA
H = 100
m = 2 * a * k ** 2

T = 20

t = c(0:20) 
o1 = l ^ 2  * H / (T * l ^ 2 + 2 * a * k ^ 2 * l ^ 2 + T) * t
o2 = H / (T * l ^ 2 + 2 * a * k ^ 2 * l ^ 2 + T) *t

data = data.frame(x = o1, y = o2, cond = "independent cost")

t = c(0:20) 
o1 = k * H / (T + 2 *a * k ^ 2) * t
o2 = 0 * t

data = rbind(data, data.frame(x = o1, y = o2, cond = "dependent cost"))
mm = max(data$y, data$x)

ggplot(data, aes(x = y, y = x, color=cond)) + 
  geom_line(aes(linetype=cond)) + 
  scale_x_continuous(name = expression(O["2"] ( a.u)), limits = c(-2, mm)) + 
  scale_y_continuous(name = expression(O["1"] ( a.u)), limits = c(-2, mm)) +
  scale_fill_brewer(palette="Set1") + 
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = c(0.7,0.5), legend.box = "vertical", 
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),      
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        text=element_text(family="Arial", size=10),
        axis.text.x =  element_text(angle = 0),
        legend.key = element_blank(),
        legend.title=element_blank()
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

ggsave(file="../graphs/conservative_field.pdf",  width=10, height=graph_h, units = "cm", device=cairo_pdf)
  
# rotational fields
k = 1
l = 1.1
a = 1
b = NA
H = 100
m = 2 * a * k ** 2

#short T
T = 7

alpha = -2*atan((tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1)/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2) + ((l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1))/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2))
v0 = (H*(l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1)*(l - 1))/(l*m - m + 2*m*tan((T*(l - 1))/(2*m)) + m*tan((T*(l - 1))/(2*m))^2 - l*m*tan((T*(l - 1))/(2*m))^2 + 2*l*m*tan((T*(l - 1))/(2*m)))
x0 = (H*l + 2*H*tan((T*(l - 1))/(2*m)) - H*l*tan((T*(l - 1))/(2*m))^2)/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
y0 = (H*(tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1))/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
w = (l -1) / m;

t = seq(0, T, 0.5)
x_t = x0 + v0 / w * sin(w * t + alpha)
y_t = y0 + v0/w * cos(w * t + alpha)

output = list()
index_r = 1
output[[index_r]] = data.frame(x = x_t, y = y_t, dur = "short_T")
index_r = index_r + 1
#draw_graph_outcome(data.frame(x = x_t, y = y_t), "short_T", "o1", "o2")


#margin T
T = m * atan(1/l) / (l - 1)  

alpha = -2*atan((tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1)/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2) + ((l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1))/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2))
v0 = (H*(l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1)*(l - 1))/(l*m - m + 2*m*tan((T*(l - 1))/(2*m)) + m*tan((T*(l - 1))/(2*m))^2 - l*m*tan((T*(l - 1))/(2*m))^2 + 2*l*m*tan((T*(l - 1))/(2*m)))
x0 = (H*l + 2*H*tan((T*(l - 1))/(2*m)) - H*l*tan((T*(l - 1))/(2*m))^2)/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
y0 = (H*(tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1))/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
w = (l -1) / m;

t = seq(0, T, 0.5)
x_t = x0 + v0 / w * sin(w * t + alpha)
y_t = y0 + v0/w * cos(w * t + alpha)

#draw_graph_outcome(data.frame(x = x_t, y = y_t), "mid_T", "o1", "o2")
output[[index_r]] = data.frame(x = x_t, y = y_t, dur = "mid_T")
index_r = index_r + 1

#large T
T = 23

t = seq(0, T -  m * atan(1/l) / (l - 1), 0.5)
x_t = 0 * t
#y_t = ((H * l) / (T * l  *l + m)) * t
y_t = ((H * (l - 1)) / ( m)) * t

T =  m * atan(1/l) / (l - 1)

alpha = -2*atan((tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1)/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2) + ((l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1))/(l + 2*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2))
v0 = (H*(l^2 + 1)^(1/2)*(tan((T*(l - 1))/(2*m))^2 + 1)*(l - 1))/(l*m - m + 2*m*tan((T*(l - 1))/(2*m)) + m*tan((T*(l - 1))/(2*m))^2 - l*m*tan((T*(l - 1))/(2*m))^2 + 2*l*m*tan((T*(l - 1))/(2*m)))
x0 = (H*l + 2*H*tan((T*(l - 1))/(2*m)) - H*l*tan((T*(l - 1))/(2*m))^2)/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
y0 = (H*(tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - 1))/(l + 2*tan((T*(l - 1))/(2*m)) + tan((T*(l - 1))/(2*m))^2 + 2*l*tan((T*(l - 1))/(2*m)) - l*tan((T*(l - 1))/(2*m))^2 - 1)
w = (l -1) / m;

t = seq(0,  T , 0.5)
x_t = c(x_t, x0 + v0 / w * sin(w * t + alpha))
y_t = c(y_t, tail(y_t, n=1) + y0 + v0/w * cos(w * t + alpha))

#draw_graph_outcome(data.frame(x = x_t, y = y_t), "long_T", "o1", "o2")
output[[index_r]] = data.frame(x = x_t, y = y_t, dur = "long_T")
index_r = index_r + 1

require(data.table)
roational_data = as.data.frame(rbindlist(output))
levels(roational_data$dur) = c("short", "medium","long")
draw_graph_outcome(subset(roational_data, TRUE),"roational_data",  expression(O["1"]( a.u)), expression(O["2"]( a.u)))

draw_graph = function(data, file_name, y_axis, y_label="response rate", 
                      graph_w=graph_w, graph_h=graph_h){
  ggplot(data, aes(x = x, y = y)) + 
    stat_summary(fun.y = "mean", geom = "line", position = position_dodge(), color = "black" ) + 
    scale_x_continuous(name = y_axis) + 
    scale_y_continuous(name = y_label) +
    scale_fill_brewer(palette="Set1") + 
    theme_bw() +
    theme(legend.direction = "vertical", legend.position = "none", legend.box = "vertical", 
          axis.line = element_line(colour = "black"),
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),      
          panel.border = element_blank(),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1),
          text=element_text(family="Arial", size=10),
          axis.text.x =  element_text(angle = 0),
          legend.key = element_blank()
    ) +
    guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
  ggsave(file=paste("../graphs/", file_name, ".pdf", sep = ""),  width=graph_w, height=graph_h, units = "cm", device=cairo_pdf)
}


draw_graph_outcome = function(data, file_name, y_axis, x_axis){
  mm = max(data$y, data$x)
  ggplot(data, aes(x = x, y = y, color=dur)) + 
    geom_line(aes(linetype=dur)) + 
    scale_x_continuous(name = x_axis) + 
    scale_y_continuous(name = y_axis) +
    coord_fixed(ratio = 1, xlim = c(-2, mm)) + 
    scale_fill_brewer(palette="Set1", guide = guide_legend(title = NULL)) + 
#    scale_color_brewer(palette="Set1", guide = guide_legend(title = NULL)) + 
    theme_bw() +
    theme(legend.direction = "vertical", legend.position = c(0.7,0.5), legend.box = "vertical", 
          axis.line = element_line(colour = "black"),
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),      
          panel.border = element_blank(),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1),
          text=element_text(family="Arial", size=10),
          axis.text.x =  element_text(angle = 0),
          legend.key = element_blank(),
          legend.title=element_blank()
    ) +
    guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
  ggsave(file=paste("../graphs/", file_name, ".pdf", sep = ""),  width=graph_w, height=graph_h * 2, units = "cm", device=cairo_pdf)
}

# some test of pareto distribution
library(SpatialExtremes)

loc = 0
scale = 18
shape = 0.7

tp = 15

# numerical approx of the expectation
dt = 0.5
i = seq(tp, 10000000, dt)
m = 0

m = sum(i * dgpd(i, loc, scale, shape))
m / (1 - pgpd(tp, loc, scale, shape)) * dt

# closed from
(scale + shape * tp) / (1 - shape) + tp