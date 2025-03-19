# This webpage is helpful for formatting these lengthy paragraphs
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
#Summary: Here lies all the in app text for the various portions within the app.
#Each page or segment of the app is delineated here.
#Refer here for text editing. 
# ==========================
# Text for welcome page
welcomeText <- tagList(
  tags$p("This platform summarizes and displays data collected by the National Marine 
  Fisheries Service (NMFS) on the permits they issue for scientific research that may 
  impact U.S. West Coast salmon, steelhead, eulachon, rockfish, and sturgeon that are 
  listed under the Endangered Species Act (ESA).",
         style = "font-size:15px"),
  tags$br(),
  tags$image(src = "image/fishJumping.png", width = "50%", height = "50%"),
  tags$br(),
  tags$br(),
  tags$p("The app summarizes how much impact is authorized to occur to ESA-listed fishes in particular
  areas due to research. Users can choose what data to display to learn about projects on a 
  particular species or in a region of interest, or look at trends over time. It was created 
  to help NMFS staff see the ‘big picture’ view of research on the landscape, let researchers 
  learn about each other’s work, and make permitting decisions more transparent.",
  style = "font-size:15px"),
  tags$br(),
  tags$image(src = "image/mapGraph2.png", width = "80%", height = "65%")
)

# ==========================
# Text/video on instructional page
how2Text <- tagList(
  tags$p("The following short video demonstrates how to use the various features of the app."),
  tags$video(id="videoID", width = "500px", height = "350px", type = "video/mp4", 
             src = "image/how2.mov", controls = "controls")
)


# ==========================
# Text for background and purpose page
backgroundText <- tagList(
  tags$h4(strong("What is the ESA?")),
  tags$p("The",
         tags$a("Endangered Species Act (ESA)", 
         href = "https://www.fisheries.noaa.gov/national/endangered-species-conservation/endangered-species-act"),
         " was enacted in 1973 to provide a policy framework for the protection and conservation of 
         threatened and endangered species:"),
  tags$ul(
    tags$li("Endangered species are species that are at risk of extinction throughout all or a 
        significant portion of its range."),
    tags$li("Threatened species are those that are likely to become an endangered species within 
        the foreseeable future throughout all or a significant portion of its range.")
    ),
    tags$p("The ESA prohibits 'take', which means to 'harass, harm, pursue, hunt, shoot, wound, kill, 
      trap, capture, or collect, or to attempt to engage in any such conduct' (16 U.S.C. 1531-1544)."),
    tags$br(),
    tags$h4(strong("How does the ESA affect scientific research?")),
    tags$p("Research on ESA-listed species is important to understand their current extinction risk and 
      threats to recovery. The ESA therefore outlines exceptions to the prohibitions on take where 
      researchers can apply for permits to conduct studies on ESA-listed species:"),
    tags$ul(
      tags$li("Under section 10(a)(1)(A) of the ESA, for studies conducted by any entity that have a 
         bona fide scientific purpose and meet other key",
         tags$a("criteria", href = "https://www.govinfo.gov/app/details/CFR-2010-title50-vol7/CFR-2010-title50-vol7-sec222-308"),
         ", such as limiting harm or justifying the data need; or"),
      tags$li("Under section 4(d) of the ESA, for research that may take threatened species being conducted 
              by state agencies or tribes.")
    ),
    tags$p("Researchers apply for permits through the Authorizations and Permits for Protected Species", 
       tags$a("(APPS)", href = "https://apps.nmfs.noaa.gov/index.cfm"), # adds a hyperlink to APPS 
       " application, and National Marine Fisheries Service (NMFS) personnel view this information and make permitting decisions based on the 
       expected impacts of the research to the species relative to the value of the information that would be collected.
       For more information on permits and authorizations under section 10(a)(1)(A) and section 4(d), see ",
  tags$a("here.", href = "https://www.fisheries.noaa.gov/west-coast/endangered-species-conservation/endangered-species-act-permits-and-authorizations-west")),
  tags$br(),
  tags$h4(strong("Why was this app developed?")),
  tags$p("Currently NMFS' West Coast Region staff do not have an easy way to map and visually summarize their research
         permitting information for internal use, or to share with applicants and co-managers.
         Therefore, students from the School of Marine and Environmental Affairs (SMEA) at the University of Washington (UW)
         were contracted with the primary purpose of developing an application that would:"),
  tags$ul(
    tags$li("Support the decision-making process for issuing scientific research permits in NMFS' West
            Coast Region,"),
    tags$li("Provide more transparency to researchers as well as state and tribal government co-managers
            about the permitting process, and"),
    tags$li("Educate the public about the role of research to inform
            the mangement of ESA-listed species.")
  ),
  tags$br(),
  tags$h4(strong("References:")),
  tags$ol(
    tags$li("Endangered Species Act. 16 U.S.C. 1531-1544 (1973)."),
    tags$li("National Marine Fisheries Service (NMFS). (2019). Chapter 3:  NMFS Pacific Marine/Anadromous Fish and 
    Invertebrates Scientific Research Authorizations and Oregon Scientific Take Permits. National Marine 
    Fisheries Service. 1315 East-West Highway Silver Spring, MD 20910.")
  )
)

# ==========================
# Text for general glossary page
glossText <- tagList(
  tags$h4(strong("Acronyms")),
    tags$li("Department of Fish and Wildlife (DFW): State agencies tasked with the protection and 
            conservation of the state’s fish, wildlife, and ecosystems for the benefit of their citizens. 
            One DFW exists for each state in NMFS' West Coast Region; Oregon Department of Fish and Wildlife
            (ODFW), Washington Department of Fish and Wildlife (WDFW), California Department of Fish and Wildlife
            (CDFW), and Idaho Department of Fish and Game (IDFG)."),
    tags$li("Distinct Population Segment (DPS): A vertebrate population or group of populations that is discrete 
            from other populations of the species and significant in relation to the entire species.", tags$sup("1")),
    tags$li("Endangered Species Act (ESA): United States federal legislation that outlines the 
            protection and conservation of the nation’s at risk species."),
    tags$li("Evolutionarily significant unit (ESU): A Pacific salmon population or group of populations 
            that is substantially reproductively isolated from other conspecific populations and that 
            represents an important component of the evolutionary legacy of the species.", tags$sup("1")),
    tags$li("Hydrologic Unit Code (HUC): A HUC is a hierarchical land area classification system created 
            by the United States Geological Survey (USGS)."),
    tags$li("National Marine Fisheries Service (NMFS): Agency within NOAA responsible for the management, 
            protection and conservation of United States marine resources."),
    tags$li("National Oceanic and Atmospheric Administration (NOAA): Scientific and regulatory agency in 
            the United States that researches weather and atmospheric conditions as well as managing 
            fisheries, marine mammals and endangered species. To learn more about NOAA look ",
            tags$a("here.", href = "https://www.noaa.gov/about-our-agency")),
  tags$br(),
  tags$h4(strong("Terms and Definitions")),
    tags$li("Dip Net: Method of capture where a long handled fishing net (up to five feet in diameter) 
            is dipped into the water to catch unsuspecting fish below."), 
    tags$li("Electrofishing: Method of capture where electrical current is used to inhibit or stun 
            fish that can then be captured."), 
    tags$li("Metadata: Represents data about data, enriches the data with information that makes it 
            easier to find, use and manage."),
    tags$li("Nomenclature: The devising or choosing of names for things, especially in a science or 
            other discipline."), 
    tags$li("Take: Defined as any action that harasses, harms, pursues, hunts, shoots, wounds, kills, 
            traps, captures, or collects listed species or attempting to engage in any such conduct.", tags$sup("1")),
    tags$li("Trap: Various capture methods (minnow trap, screw trap, incline plane trap, etc.) that involve the setup 
            of a device that passively catches fish and accumulates them into a live holding tank or compartment."), 
    tags$li("Trawl: Capture method that involves pulling a large net through the water using one or 
            more boats."), 
    tags$li("Seine: A net with floats on top and weights on the bottom that encircles the fish. Can 
            be used from shore (beach seine) or from a boat (purse seine)."), 
  tags$br(),
  tags$h6(strong("Sources", style = "font-size=10px")),
    tags$ol(
      tags$li(tags$a("https://www.fisheries.noaa.gov/laws-and-policies/glossary-endangered-species-act#:~
                           :text=Take%20as%20defined%20under%20the,%2C%20but%20not%20unexpected%2C%20taking.",
                     href = "https://www.fisheries.noaa.gov/laws-and-policies/glossary-endangered-species-act#:
                     ~:text=Take%20as%20defined%20under%20the,%2C%20but%20not%20unexpected%2C%20taking.")),
    style = "font-size:10px"),
  tags$br(),
  tags$h4(strong("Other key terms")),
    tags$li("Dip Net: method of capture where a long handled fishing net (up to five feet in diameter) is dipped into the water to catch unsuspecting fish below."), 
    tags$li("Electrofishing: Method of capture where electrical current is used to inhibit or stun fish that can then be captured."), 
    tags$li("Metadata: represents data about data, enriches the data with information that makes it easier to find, use and manage."),
    tags$li("Nomenclature: The devising or choosing of names for things, especially in a science or other discipline."), 
    tags$li("Take: Defined as any action that harasses, harms, pursues, hunts, shoots, wounds, kills, traps, captures, or collects, or attempts to engage in any such conduct."),
    tags$li("Trap: Various capture methods (screw trap, incline plane trap) that involve the setup of a device that passively catches fish and accumulates them into a live holding tank."), 
    tags$li("Trawl: Capture method that involves pulling a large net through the water using one or more boats."), 
    tags$li("Seine: a net with floats on top and weights on the bottom that encircles the fish. Can be used from shore (beach seine) or from a boat (purse seine)."), 
  tags$br(),
  tags$h4(strong("Fish Glossary")),
    tags$p("Below are the names of the fish species that are included within this app. They are the fish species
           under NMFS' jurisdiction for which an ESU or DPS is currently listed as threatened or endangered
           under the ESA."), 
  tags$h5(strong("Salmonids:")),
           tags$li("Chinook Salmon", em("(Oncorhynchus tshawytscha)")),
           tags$li("Coho Salmon", em("(Oncorhynchus kisutch)")),
           tags$li("Chum Salmon", em("(Oncorhynchus keta)")),
           tags$li("Sockeye Salmon", em("(Oncorhynchus nerka)")),
           tags$li("Steelhead Trout", em("(Oncorhynchus mykiss)")),
  tags$p("For more information regarding these species see ",
         tags$a("here.", href = "https://www.fisheries.noaa.gov/species/pacific-salmon-and-steelhead")),
  tags$h5(strong("Other species:")),
           tags$li("Boccaccio Rockfish",em("(Sebastes paucispinis)")),
           tags$li("Eulachon", em("(Thaleichthys pacificus)")),
           tags$li("Green Sturgeon", em("(Acipenser medirostris)")),
           tags$li("Yelloweye Rockfish", em("(Sebastes ruberrimus)")),
  tags$p("Find more information regarding these species see ",
           tags$a("here.", href = "https://www.fisheries.noaa.gov/species-directory/threatened-endangered?oq=&field_species_categories_vocab=1000000031&field_species_details_status=All&field_region_vocab=1000001126&items_per_page=25"))
           
)

# ==========================
# Text for use and limitations page
disclaimerText <- tagList(
  tags$h4(strong("Things to know about this app")),
  tags$p("Users of this app should be aware of the following limitations and assumptions regarding the raw data and data summaries
         presented in this application:"),
    tags$p("These data are provisional, and are subject to change at any time. Additionally, 
            this app is specifically for ESA-listed fish species in the", 
           tags$a("west coast region", href = "https://www.fisheries.noaa.gov/about/west-coast-region"),
            "under NOAA jurisdiction. Therefore, ESA-listed fish species under the jurisdiction of the U.S. Fish and Wildlife
            Service or proteced under other state and international organizations are not represented here."),
  tags$br(),
  tags$p("For the purpose of this project, some fields and data entries were modified to 
          simplify analyses and provide consistency across the nomenclature. These include:"),
  tags$li("Adjusting HUC 8 codes to encompass redrawn boundaries; see",
    tags$a("metadata", href = "https://github.com/rory-spurr/ESAPermitsCapstone/tree/main/docs/Metadata"),
    "for details"),
  tags$li("Renaming and classifying waterbodies in the 'WaterbodyName' field to allow for consistent
          nomenclature and inform users about the type of waterbodies (saltwater or freshwater) these locations describe. 
          Renaming practices were performed using best available data provided by the 'LocationDescription' field."),
  tags$li("Reclassifying 'Lifestage' and 'Production' fields to reduce the number of unique entries. For example:"),
  tags$ul(
    tags$li("E.g. 'Smolt' and 'fry' were replaced by 'Juvenile'"),
    tags$li("E.g. 'Listed Hatchery, Clipped and Intact', 'Listed Hatchery Adipose Clip'
            'Listed Hatchery Intact Adipose' were replaced by 'Hatchery'")),

  tags$p("Details of the rule sets used to create these fields can be found within the script files 
         accessible through the github repository."),
  tags$br(),
    tags$p("Some data limitations were beyond the developers' control. 
           These include:",
      tags$li("Take may have occured which was not reported, and any take not submitted through the APPS system would not 
              be captured.")),
      tags$li("The input of incorrect HUC 8 codes by researchers applying for permits (for example: the ‘9999999’ 
              HUC codes seen in the reactive data table) meant these data could not be accurately plotted with the other map data."),
  tags$br(),
    tags$p("For the purpose of this project, the following data types were intentionally omitted:",
    tags$p("Some data limitations were beyond the developers' control. These include:",
      tags$li("Unreported take from projects for which exit reports were not completed by permittees.")),
      tags$li("The input of incorrect HUC 8 codes by researchers applying for permits (for example: the ‘9999999’ HUC codes seen in the reactive data table)"),
    tags$p("For the purpose of this project, the following were omitted:",
      tags$li("Unlisted hatchery fish."),
      tags$li("Non-invasive take actions (e.g., snorkel surveys, dead tissue samples)"),
      tags$li("Unknown take actions."),
      tags$li("Permits that are expired or were never issued.")),
      tags$li("Research happening across a large geographic scope (whole states or coasts) were omitted from the map,
              but are included in the time series. "),
      tags$li("Tribal 4d permits were omitted from tables showing individual permit information for data privacy reasons, 
              but included in the totals shown in the map and time series."),
  tags$br(),
  tags$h4(strong("Metadata")),
    tags$p("For further information regarding the data source, data attributes, 
         R packages used, and general metadata for this project, please visit the 
          metadata folder of our Github ",
    tags$a("here.", href = "https://github.com/rory-spurr/ESAPermitsCapstone/tree/main/Metadata")), 
  tags$br(),
  tags$h4(strong("Citation")), 
  tags$p("Spurr, R., & Santana, A. (2023). Visualizing ESA-Listed Fish Research on the West Coast (Version 1.0.0) 
          [Computer software]. https://github.com/rory-spurr/ESAPermitsCapstone"),
  tags$br(),
  tags$h4(strong("License Statement")),
    tags$image(src = "image/by-nc.png", width = "15%", height = "10%", align = "center"),
  
    tags$p("CC BY-NC: This license allows reusers to distribute, remix, adapt, and build upon the material in any medium or format for noncommercial purposes only, and only so long as attribution is given to the creator.")),
  tags$br(),
  tags$br(),
  tags$h4(strong("Datasets Cited")),
  tags$p("National Marine Fisheries Service and Oregon Department of Fish and Wildlife. 
         Authorizations and Permits for Protected Species (APPS). Current authorizations 
         for research under ESA Section 10(a)(1)(A) and Section 4(d), and reported take 
         from 2012-2023, for fish species in Washington, Oregon, Idaho, and California. 
         Available online at https://apps.nmfs.noaa.gov/. Accessed 02/09/23."),
  tags$image(src = "image/download.png", width = "20%", height = "20%", align = "center"),
  tags$h4(strong("Acknowledgements")),
    tags$p("We thank Diana Dishman for her conceptualization of the project, expert technical 
    guidance surrounding the permitting process and editing and critiquing drafts of the application. 
    We also want to thank Anne Beaudreau for her help editing and critiquing drafts of the 
    application, as well as the professional and technical guidance she has shown us throughout 
    our time in grad school. We want to thank both NMFS' West Coast Region research permit and communications 
    teams for taking the time to meet with us and help with the application. Funding for this 
    project was provided through NMFS West Coast Region Protected Resources Division, as well as 
    the Jay Ginter Memorial Scholarship Fund at the University of Washington.")
  )

# ==========================
# Text for about Map section
aboutMapTxt <- tags$p("The map displays total authorized take (or total lethal take depending on what data 
       is selected to display), for each Hydrologic Unit Code 8 (HUC 8). The black outline shows
       all the possible HUC 8's where these species may be encountered. Individual HUC 8's may be
       clicked on inside the map to reveal more information for the HUC, including specific 
       authorized take values.")

# ==========================
# Text for Map table caption
tblCaptText <- tagList(
  tags$p("This table displays the raw data from the map above. The fields 
         within this table are defined as:"),
  tags$li("Permit Code: The code automatically assigned by the APPS system and used in correspondence about the application.
          Can be searched on APPS to learn more about an individual permit."),
  tags$li("Permit Type: Indicates the kind of permit or authority ",
          tags$a("(more detail here).", href =  "https://www.fisheries.noaa.gov/west-coast/endangered-species-conservation/endangered-species-act-permits-and-authorizations-west")),
  tags$li("Organization: Entity in charge of research operations."),
  tags$li("HUC 8: Hydrologic Unit 8 (HUC 8) code where research is taking place."),
  tags$li("Location: Name or description of research location."),
  tags$li("Water Type: Whether the research is happening in freshwater (FW) or saltwater (SW)."),
  tags$li("Take Action: Action taken to capture/tag/kill animals."),
  tags$li("Gear Type: Gear used to capture species."),
  tags$li("Total Take: Total amount of take authorized. Includes both lethal and non-lethal take."),
  tags$li("Lethal Take: Total amount of fish mortality authorized.")
)

# ==========================
# Text for time series glossary
TSglossText <- tagList(
  tags$p("This table displays the raw data from the plots above. The fields 
         within this table are defined as:"),
    tags$li("Year: The year field indicates the year that each permit was issued."),
    tags$li("Report ID: A five-digit unique code for each active project that reported take and mortality within their research."),
    tags$li("Permit Code: The code automatically assigned by the APPS system and used in correspondence about the application."),
    tags$li("Permit Type: Indicates the kind of permit or authority ",
            tags$a("(more detail here).", href =  "https://www.fisheries.noaa.gov/west-coast/endangered-species-conservation/endangered-species-act-permits-and-authorizations-west")),
    tags$li("Gear Type: Gear used to capture species."),
    tags$li("Authorized Take: Predicted and allocated number of individuals the project expects to take as a result of research. Includes numbers from lethal and non-lethal take."),
    tags$li("Reported Take: The number of individuals reportedly taken as a result of research. Includes numbers from lethal and non-lethal take."),
    tags$li("Unused Take: The number of fish authorized to be taken that was allocated but went unused by the researcher. The difference between the authorized number and the reported number."),
    tags$li("Authorized Mortality: Authorized number of individuals that may be killed as a result of research. Includes ONLY lethal take."),
    tags$li("Reported Mortality: The number of individuals reportedly killed as a result of research. Includes ONLY lethal take."),
    tags$li("Unused Mortality: The number of fish authorized to be killed that went unused by the researcher. The difference between the authorized number and the reported number.")
)

# Text for user boxes
# ==========================

# Rory
roryText <- tagList(
  p("Rory Spurr has worn many hats throughout his life, working as a fisheries scientist, in research
    and development for a fish passage company, and most recently as an R programmer. Rory will graduate with 
    his Master's in March and is excited to see where his career will take him."),
  tags$br(),
  h1(strong("Contact Info:"), style = "font-size:25px"),
  p(tags$a("Website", href = "https://rory-spurr.github.io/")),
  p("Github: ", tags$a("rory-spurr", href = "https://github.com/rory-spurr")),
  p("Email: rjspurr5@live.com")
)

# =========================

# Alana 
alanaText <- tagList(
  p("Alana Santana is a multifaceted individual, specializing in several areas including 
    fish and marine biology, conservation, biomechanics, R coding, and ArcGIS. After 
    Alana obtains her Master’s, she is interested in pursuing a career that
    allows her to integrate coding and spatial analysis skills into her work for conservation purposes."),
  tags$br(),
  h1(strong("Contact Info:"), style = "font-size:25px"),
  p("Github: ", tags$a("asantan8", href = "https://github.com/asantan8")),
  p("Email: asantan7@uw.edu")
)








