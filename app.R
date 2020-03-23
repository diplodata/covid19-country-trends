# **** COVID19CountryTrends ****

# FCO Open Source Unit
# OpenSourceUnit@fco.gov.uk

require(shiny)
require(tidyverse)
require(countrycode)

# setwd('~/Documents/Coronavirus/COVID19CountryTrends')

# The AnthonyEbert repo lookup is for ISO country code lookups, given raw JH data doesn't offer codes and country names have been unstable
cc = countrycode::codelist %>% as_tibble()
cc = read_csv('https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/JohnsHopkins-to-A3.csv', col_types='cc') %>% janitor::clean_names() %>% 
  left_join(select(cc,iso2c,iso3c), by = c(alpha3 = 'iso3c')) %>% 
  add_case(country_region = 'Kosovo', alpha3 = 'RKS', iso2c = 'XK')

pop_data = read_csv('pop_data.csv', na = '', col_types = 'ccd') %>% 
  add_case(country = 'Taiwan', iso2c = 'TW', pop = 23780452) %>% 
  add_case(country = 'French Guiana', iso2c = 'GF', pop = 290691) %>% 
  add_case(country = 'Martinique', iso2c = 'MQ', pop = 376480) %>% 
  add_case(country = 'Réunion', iso2c = 'RE', pop = 859959)

d = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-' %>% 
  paste0(c('Confirmed.csv','Deaths.csv','Recovered.csv'))
d = set_names(as.list(d), str_extract(d, '[^-]+(?=.csv)'))
d0 = suppressMessages(map_df(d, read_csv, .id = 'set') %>% select(-Lat, -Long))

d = d0 %>% pivot_longer(4:ncol(.), names_to = 'date', values_to = 'total_cases') %>% 
  janitor::clean_names() %>% mutate(date = as.Date(date, format = '%m/%d/%y')) %>% 
  group_by(set, province_state, country_region) %>% 
  mutate(n = c(0, diff(total_cases))) %>% ungroup() %>% 
  left_join(cc, by = 'country_region') %>% 
  left_join(select(pop_data, iso2c, pop), by = 'iso2c')


# any countries with national AND privincial data?
d %>% mutate(na_prov = is.na(province_state)) %>% count(country_region, na_prov) %>% 
  count(country_region, sort=T) %>% filter(n > 1) %>% na.omit %>% nrow %>% 
  assertthat::are_equal(1) %>% if(.){ message('*** COUNTRY HAS NATIONAL AND PROVINCIAL DATA ***') }

dn = d %>% group_by(set, country_region,iso2c, date) %>% 
  summarise(total_cases = sum(total_cases, na.rm=T), n = sum(n, na.rm=T), pop = first(pop)) %>% ungroup %>% 
  arrange(country_region, set, date) #%>% 
  #filter(!is.na(pop))

message('Mis-matches in country names:')
print(setdiff(sort(unique(d$country_region)), unique(dn$country_region)))

country_groups = jsonlite::fromJSON('regions.json')

# lab_range = function(x) pretty(x) %>% .[. < max(x)] %>% max() %>% c(0,.)
lab_range = function(x) pretty(c(0,x), n = 2) %>% .[. <= max(x)]
set_lims = function(x) c(0, max(x))

# select mode-appropriate method for aggregating value timeline values by week/month
agg_fun = function(plot_stat){
  case_when(
    # str_detect(plot_stat, fixed('total', ignore_case=T)) ~ list(dplyr::last),
    str_detect(plot_stat, regex('(ratio)|(rate)|(total)', ignore_case=T)) ~ list(dplyr::last),
    TRUE ~ list(base::sum)
  )[[1]]
}

modes = c(`Total cases timeline` = 'Timeline plot of the cumulative country counts of cases',
          `Total cases ranking` = 'Rank plot of cumulative country counts of cases (of the first of the selected case types)',
          `Total cases per 10,000 population timeline` = 'Timeline plot of cumulative country counts of cases, balanced by population size',
          `Total cases per 10,000 population ranking` = 'Rank plot of the cumulative country counts of cases (of the first of the selected case types), balanced by population size',
          `New cases timeline` = 'Count of newly-confirmed infections or medical resolutions',
          `New cases per 10,000 population timeline` = 'Count of newly-confirmed infections or medical resolutions, balanced by population size',
          `Live cases timeline` = 'Count of live confirmed infections pending a medical resolution (recovery or death). This statistic may over-estimate the numbers if the numbers of e.g. recovered cases are not accurately reported by any countries.',
          `Resolved / Confirmed ratio timeline` = 'Calculated as cumulative totals of (Recovered + Deaths) / Confirmed (values range 0-1).  This offers insight into an epidemic\'s development. Accurate values must approach 1 eventually - reflecting epidemic growth checked by herd immunity and/or a successful containment strategy.  But treat with caution as trends can be noisy and e.g. falsely suggest an early peak if an early outbreak is contained before the virus achieves widespread transmission.',
          `Case Fatality Rate` = 'CFR is the percentatage of total confirmed infections that are recorded fatalities.  Do not confuse CFR with mortality rate (deaths as a proportion of total population). CFR doesn\'t factor for undiagnosed cases, so will be higher than the COVID-19\'s true fatality rate. Note CFR is also influenced by various factors (e.g. cultural, geographical, bureaucratic) that determine which infections are ultimately diagnosed.',
          `Date-adjusted timeline` = 'Timeline plot with date-adjusted x-axis representing days since the country reached 50 cases (of the first of the selected case types). Log scale is recommended. This mode ignores date range, and time aggregation is not recommended.  For better original implementation see https://kiksu.net/covid-19/'
          )

#----------------------------


server = function(input, output, session){
  
  observeEvent(input$logscale, if(input$logscale) updateRadioButtons(session = session, inputId = 'yscale', selected = 'fixed'))
  
  observe({
    # invalidateLater(2000, session)
    
    # params
    plot_stat = input$mode
    date_adjusted = str_detect(plot_stat, fixed('Date-adjusted', ignore_case=T))
    plot_sets = input$cases
    if(length(plot_sets) == 0) plot_sets = c('Deaths','Recovered','Confirmed')
    date_min = input$date_range[1]
    date_max = input$date_range[2]
    scale_y_custom = ifelse(input$yscale == 'log', list(scale_y_log10), list(scale_y_continuous))[[1]]
    
    output$plot = renderPlot({
      # jsonlite::write_json(reactiveValuesToList(input), path = 'input.json')
      # input = jsonlite::read_json('input.json') %>% map(~ unlist(.x))
      
      # filtering and statistic calculation
      dat = dn
      if(input$countries != 'World'){
        if(input$countries %in% names(country_groups)){
          countries = country_groups[[input$countries]]
        } else countries = input$countries
        dat = dat %>% filter(country_region %in% countries)
      }
      
      if(!date_adjusted) dat = dat %>% filter(date >= date_min, date <= date_max)
      
      # TIMELINE PLOTS
      
      if(str_detect(plot_stat, fixed('timeline', ignore_case=T))){
        
        # Process data for the selected statistic mode
        if(plot_stat %in% names(modes)[c(1,3,5,6,10)]){ # simple timeline plots (raw data)
          
          if(length(plot_sets) < 3) dat = dat %>% filter(set %in% plot_sets)
          if(str_detect(plot_stat, fixed('Date-adjusted', ignore_case=T))) dat = dat %>% filter(set == plot_sets[1]) # Date-adjusted plots are single set only
          
          dat = dat %>% mutate(stat = case_when(
            plot_stat %in% c('Total cases timeline', 'Date-adjusted timeline') ~ total_cases,
            plot_stat == 'New cases timeline' ~ n,
            plot_stat %in% c('Total cases per 10,000 population timeline', 'Date-adjusted timeline per 10,000 population') ~ 1e4 * total_cases / pop,
            plot_stat == 'New cases per 10,000 population timeline' ~ 1e4 * n / pop
          ))
        }
        if(plot_stat == 'Live cases timeline'){
          dat = dat %>% select(-n, -pop) %>% pivot_wider(names_from = set, values_from = total_cases) %>% 
            mutate(stat = Confirmed - Deaths - Recovered, set = 'Confirmed', pop=0)
        }
        if(plot_stat == 'Resolved / Confirmed ratio timeline'){
          country_fil = dat %>% filter(set == 'Recovered') %>% arrange(-as.numeric(date)) %>% 
            group_by(country_region) %>% slice(1) %>% ungroup %>% 
            filter(total_cases >= 10) %>% .[['country_region']]
          
          dat = dat %>% filter(country_region %in% country_fil) %>% 
            select(-n, -pop) %>% pivot_wider(names_from = set, values_from = total_cases) %>% 
            filter(Recovered >= 5) %>% 
            mutate(stat = (Recovered + Deaths) / Confirmed) %>% 
            mutate(stat = replace(stat, is.nan(stat), 0), set = 'Confirmed', pop = 0)
        }

        # time aggregation
        if(tolower(input$time_agg) != 'day'){
          dat = dat %>% mutate(date = lubridate::floor_date(date, tolower(input$time_agg))) %>% 
            group_by(set, country_region, iso2c, date) %>% 
            summarise(stat = agg_fun(plot_stat)(stat), pop = first(pop)) %>% ungroup
        }
        
        # countries to include and their plot order
        country_order = dat %>% group_by(country_region) %>% arrange(-as.numeric(date)) %>% slice(1) %>% 
          ungroup %>% arrange(-stat) %>% filter(stat > 0) %>% .[['country_region']] %>% .[.!="Others"]
        
        # filter top cases and set factors
        n_countries = min(input$n, length(unique(dat$country_region)))
        
        dat = dat %>% mutate(set = factor(set, levels = c('Deaths','Recovered','Confirmed')),
                       country_region = fct_other(country_region, keep = head(country_order, n_countries-1), other_level = 'Others') %>% 
                         factor(levels = c(head(country_order, n_countries-1), 'Others'))) %>% 
          group_by(country_region, set, date) %>% summarise(stat = sum(stat), pop = sum(pop)) %>% ungroup()
        
        if(date_adjusted) dat = dat %>% filter(stat >= 50) %>% 
          group_by(country_region) %>% mutate(date = as.numeric(date - first(date))) %>% ungroup() %>% 
          filter(country_region != 'Others')
        
        dat_labs = dat %>% group_by(country_region) %>% 
          filter(stat == max(stat)) %>% slice(1) %>% ungroup() %>% 
          mutate(lab = scales::comma(base::round(stat,2), accuracy = ifelse(sum(dat$stat - round(dat$stat,0), na.rm = TRUE) == 0, 1, 0.01))) %>% 
          arrange(-stat)
        
        if(date_adjusted) dat_labs = dat_labs %>% mutate(lab = paste(country_region, lab))
        
        if(date_adjusted){ p = dat %>% ggplot(aes(date, stat, col = country_region))
        } else p = dat %>% ggplot(aes(date, stat, col = set))
        
        # ggplot
        p = p + geom_line(size = .75) + 
          ggrepel::geom_text_repel(data = dat_labs, 
                                   aes(date, stat, label = lab),
                                   direction = 'x', nudge_x = -1, segment.size = 0) +
          labs(col = NULL, x = NULL, y = ifelse(str_detect(plot_stat, fixed('ratio', ignore_case=T)),'Ratio','Cases')) + 
          scale_y_custom() +
          scale_color_discrete(guide = guide_legend(reverse = T)) +
          expand_limits(y = 0) +
          ggthemes::theme_pander() +
          theme(legend.position = 'top', plot.margin = unit(rep(3,4),'mm'), legend.key=element_rect(fill=NA)) +
          guides(colour = guide_legend(override.aes = list(size = 4)))
        
        if(!date_adjusted){
          p = p + facet_wrap(~ country_region, scales = ifelse(input$yscale == 'free_y', 'free_y', 'fixed')) +
            guides(colour = guide_legend(override.aes = list(size = 4)))
        } else p = p + guides(colour = FALSE)
        p
        
      } else {
        
        # OTHER PLOTS
        
        dat = dat %>% arrange(date) %>% group_by(country_region, set, iso2c) %>% 
          summarise(pop = first(pop), total_cases = last(total_cases)) %>% 
          pivot_wider(names_from = set, values_from = total_cases) %>% ungroup %>% 
          filter(Confirmed > 0)
        
        dat = dat %>% mutate(stat = case_when(
          plot_stat == 'Case Fatality Rate' ~ 100 * Deaths / Confirmed,
          plot_stat == 'Total cases ranking' & 'Confirmed' %in% plot_sets ~ Confirmed,
          plot_stat == 'Total cases ranking' & all(c('Recovered','Deaths') %in% plot_sets) ~ Recovered + Deaths,
          plot_stat == 'Total cases ranking' & 'Recovered' %in% plot_sets ~ Recovered,
          plot_stat == 'Total cases ranking' & 'Deaths' %in% plot_sets ~ Deaths,
          plot_stat == 'Total cases per 10,000 population ranking' ~ 1e4 * Confirmed / pop
        )) %>% arrange(-stat)
        
        if(plot_stat == 'Case Fatality Rate') dat = dat %>% filter(Confirmed > 10, Deaths > 3)
        
        dat = dat %>% slice(1:input$n)
        
        p = dat %>% mutate(lab = scales::comma(base::round(stat,2), accuracy = ifelse(sum(dat$stat - round(dat$stat,0)) == 0, 1, 0.01))) %>% 
          ggplot(aes(fct_reorder(country_region, stat), stat, col=stat, fill=stat)) +
          geom_col(width=.8) + 
          geom_text(aes(label = lab), nudge_y = .03 * ifelse(input$yscale == 'log', log10(max(dat$stat)), max(dat$stat)) ) +
          coord_flip() +
          theme_void() +
          theme(axis.text.y = element_text(), 
                panel.grid.major.x = element_line(color='grey80'),
                panel.grid.minor.x = element_line(color='grey80', linetype = 'dotted'),
                axis.text.x = element_text(),
                plot.margin = unit(rep(3,4),'mm')) +
          scale_y_custom(labels = scales::comma) +
          scale_color_continuous(low = 'grey50', high = 'red') +
          scale_fill_continuous(low = 'grey50', high = 'red') +
          guides(col=F, fill=F)
        
        p
      }
      
    })
    
    output$plot_title = renderText(plot_stat)
    output$plot_desc = renderText(modes[plot_stat])
    
  })
  
}



ui = fluidPage(
  tags$style(type='text/css', "
.selectize-dropdown-content {max-height: 400px;}
#yscalecontrol, #yscalecontrol div { margin: 0px !important; padding: 0px !important; }
"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(width = 3,
                 h3('COVID-19 Tracker'),
                 sliderInput("date_range", "Date range", min=as.Date("2020-01-01"), max=Sys.Date(), 
                             value = c(Sys.Date()-21, Sys.Date()), step = 1, dragRange = T,
                             timeFormat = '%d %b'),
                 selectInput('mode', 'Statistic', names(modes)),
                 selectInput(inputId = 'countries', label = 'Countries, regions & groups', 
                             choices = c('World', names(country_groups), sort(unique(dn$country_region)) )),
                 checkboxGroupInput("cases", "Case type",  c("Confirmed", "Recovered", "Deaths"), selected = c("Confirmed", "Recovered", "Deaths"), inline = T),
                 sliderInput("n", "Top countries:", min = 1, max = 66, value = 20),
                 radioButtons('time_agg', 'Time aggregation', c('Day','Week','Month'), inline=T),
                 radioButtons('yscale', 'Y-scale control', c(`Linear-fixed` = 'fixed', `Linear-free` = 'free_y', `Log-fixed` = 'log'), inline=T),
                 br(), hr(),
                 p(strong('Source: '), a('Johns Hopkins University Center for Systems Science and Engineering', href='https://github.com/CSSEGISandData/COVID-19', target='_BLANK'), ' (based on statistics from WHO and national sources)')
                ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
      span(inline=T,
        h4(textOutput("plot_title")),
        p(textOutput("plot_desc"))
      ),
      plotOutput('plot', height = '85vh')
    )
  )
)


shinyApp(ui,server, options = list(launch.browser = TRUE, display.mode ='normal'))

