library(shiny)
library(shinythemes)
library(shinylive)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$style(HTML(
    "body { margin-top: 20px; }
   .custom-paragraph { text-align: left; margin: 5px auto; max-width: 700px; }
   .custom-list { margin-left: 40px; }
   .section-title { font-weight: bold; margin-top: 20px; }
   p { text-indent: 2em; } /* General paragraphs */
   #viewer-container {
     width: 100%;
     height: calc(100vh - 50px); /* Full viewport height minus the Back button height */
     overflow: hidden;
   }
   #viewer-container iframe {
     width: 100%;
     height: 100%;
     border: none;
   }
   .btn-glow {
     border: 2px solid red;
     box-shadow: 0 0 15px red;
     animation: glow 1.5s infinite;
   }
   @keyframes glow {
     0% { box-shadow: 0 0 5px red; }
     50% { box-shadow: 0 0 15px red; }
     100% { box-shadow: 0 0 5px red; }
   }
   
   h3 {
     margin-bottom: 10px;
     text-align: left;
   }
   .doc-container {
     display: flex; 
     justify-content: flex-start;
     align-items: flex-start; 
     flex-wrap: wrap;
     max-width: 700px;
     margin: 20px 0; /* Adds space below the instructional text */
     gap: 20px;
   }
   .doc-container div {
     text-align: center;
     margin: 10px;
   }
   .doc-container div p {
     margin-top: 5px;
     text-align: left;
     text-indent: 0;
   }
   .main-panel-content {
     text-align: left; 
     margin: 5px auto; 
     max-width: 700px;
   }
  "
  )),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "text-align: left; 
        margin-bottom: 15px;
        margin-top: -20px;",
        h4("Geography 690 Portfolio"),
        h5("Michael Peckham", style = "margin-bottom: 25px;"),
      ),
      h6("Navigation", style = "margin-top: 25px;"),
      navlistPanel(
        tabPanel("Introduction", value = "introduction"),
        tabPanel("Employment Package", value = "employment_package"),
        tabPanel("Work Portfolio", value = "portfolio"),
        tabPanel("Informational Interview", value = "informational_interview"),
        tabPanel("LinkedIn", value = "linkedin"),
        tabPanel("AI & Social Media", value = "ai_social_media"),
        tabPanel("Career Pathways & Grad School", value = "career_pathways_grad_school"),
        tabPanel("Action Plan", value = "action_plan"),
        id = "tabs",
        widths = c(12, 12),
        well = FALSE # Removes the border constraint
      ),
      width = 3
    ),
    mainPanel(
      uiOutput("mainOutput"), # Placeholder for dynamic content
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value for selected document
  selected_doc <- reactiveVal(NULL)
  
  # Switch to the document viewer when thumbnails are clicked
  observeEvent(input$view_coverletter, {
    selected_doc("CoverLetter_Portfolio.pdf")
    updateTabsetPanel(session, "tabs", selected = "document_viewer")
  })
  
  observeEvent(input$view_resume, {
    selected_doc("Peckham_SewerAI_Resume.pdf")
    updateTabsetPanel(session, "tabs", selected = "document_viewer")
  })
  
  observeEvent(input$view_references, {
    selected_doc("ReferencesExample.pdf")
    updateTabsetPanel(session, "tabs", selected = "document_viewer")
  })
  
  # Back to list: Switch back to Employment Package tab
  observeEvent(input$back_to_list, {
    selected_doc(NULL)
    updateTabsetPanel(session, "tabs", selected = "employment_package")
  })

  observeEvent(input$back_to_list, {
    selected_doc(NULL) # Clear the selected document
  })
  
  observeEvent(input$tabs, {
    if (input$tabs != "employment_package") {
      selected_doc(NULL) # Clear the selected document if leaving the Employment Package tab
    }
  })
  
  observeEvent(input$goto_research, {
    updateTabsetPanel(session, "portfolio_subtabs", selected = "research")
  })
  
  observeEvent(input$goto_analysis, {
    updateTabsetPanel(session, "portfolio_subtabs", selected = "analysis")
  })
  
  observeEvent(input$goto_static, {
    updateTabsetPanel(session, "portfolio_subtabs", selected = "static")
  })
  
  observeEvent(input$goto_interactive, {
    updateTabsetPanel(session, "portfolio_subtabs", selected = "interactive")
  })
  
  # When viewing CCNZ
  observeEvent(input$view_ccnz, {
    selected_doc("21st Century Climate in New Zealand.pdf")
    updateTabsetPanel(session, "tabs", selected = "document_viewer")
  })
  
  # When viewing Mt Shasta
  observeEvent(input$view_shasta, {
    selected_doc("MtShasta.pdf")
    updateTabsetPanel(session, "tabs", selected = "document_viewer")
  })
  
  # Back to Research Papers
  observeEvent(input$back_to_research, {
    selected_doc(NULL)
    updateTabsetPanel(session, "portfolio_subtabs", selected = "research")
  })
  
  # Document Viewer Output
  output$documentViewer <- renderUI({
    req(selected_doc())
    tagList(
      actionButton("back_to_research", "Back to Research Papers", class = "btn btn-secondary btn-glow"),
      tags$iframe(
        src = selected_doc(),
        width = "100%",
        height = "800px",
        frameborder = "0",
        style = "margin-top: 10px;"
      )
    )
  })
  
  
  
  
  output$mainOutput <- renderUI({
    if (!is.null(selected_doc())) {
      tagList(
        actionButton("back_to_list", "Back to Employment Package", class = "btn btn-secondary btn-glow"),
        div(
          id = "viewer-container", # Container for full-screen iframe
          tags$iframe(
            src = selected_doc(),
            width = "100%",
            height = "100%",
            frameborder = "0"
          )
        )
      )
    } else {
      switch(input$tabs,
           "introduction" = tagList(
             div(class = "custom-paragraph",
             h3("Introduction"),
             p("Over the last semester I’ve recognized the importance of having a LinkedIn, Resume, easily tailorable cover letter, and I think the most important, a quick and easy way to share my portfolio.  The exercise of compiling my portfolio was by far the most useful to me.  It was yet another example of why it is important to organize the data from the start, so that a year later when I need it, it is easy to find.  If an opportunity arises now I have the things ready to go at any given moment.  The class discussions and the varying experiences from the students gave me some critical insight.  Things like having cloud access to all of the potentially requested items for any job opportunity anytime is what I really learned to be of the most importance.  While I have yet to put it all on linkedIn I have made it accessible via google drive and I plan to build it into my website.  The course also made me recognize how important having a website is, which I currently have but it is complete trash and needs to be revamped.  That's another thing the course taught me, having an online presence is inevitable in this day and age, so I might as well get with it."),
             p("Using LinkedIn was also a big learning experience for me.  I struggle with social media but “wasting” time on linkedIn just feels different.  Instead of endless scrolling through nonsense, I am now scrolling through GIS professionals and leaders of the industries I am aiming to work in.  It is so easy to connect with other professionals and an indirect learning of this class was having an easy to copy and paste connection request message on hand is key.  I’ve connected with a few other folks during the 30 day map challenge (which I only made it to day 9) but it allowed me to get some exposure to what people are doing on the leading edge.  I was also exposed to some potentially interesting jobs on linked in and since I already had everything ready to go from this class, I just applied to see what happens, to learn the process, its very easy to do.  I noticed that some job postings even introduce you to the hiring manager with a link to their LinkedIn.  Needless to say it is a very useful tool in the world we live in today.  While I am not exploring grad school options at this moment, it would obviously be just as useful to do my homework on programs and professors."),
             p("The personal growth challenges remained the same for me over the semester.  Time management and finding some level of balance, and of course the difficulty I have saying no to work projects when I have no time all remain a challenge.  For two months I stopped my weekly running routine because of work and school, this weekend I got back to it, so I am starting to find the balance again now.  However, I am hoping that after this last semester is over I will be able to find some more of that balance again.  Staying healthy, eating and sleeping good, and exercising all really help in building confidence in all the other areas of my life.  So long as I continue to do that I believe that I will be applying the skills and knowledge I developed, thanks to this course, in getting new clients and getting a sustainable and balanceable workload for 2025.  I really attribute this course with opening the networking, marketing, and connections box.  It has served as a solid foundation to entering the professional world with a higher education as the basis."),
             p("Going forward I plan to utilize the portfolio, resume, and cover letter as well as LinkedIn to showcase my work.  I appreciate having these as assignments, I love assignments that directly apply to the real world. After graduation and over the next few months my plans are to really hone in the way to quickly and easily share digital copies of my portfolio that are fun and engaging to look at.  I plan to build a section out on my website to host interactive maps - which are useless in pdf format - and to create some abstracts for my research papers so whoever is looking at my portfolio can get the big picture quickly without reading a 10 page paper.  I am going to use the final portfolio project to try and really narrow down my best pieces of work.  I want to find the balance of less is more, understanding that no one is going to look through a 100 page portfolio.  I want to try and apply the resume / cover letter logic of short and sweet / clear, concise, and to the point to the portfolio.")
           )  
           ),
           
           "employment_package" = tagList(
             div(class = "main-panel-content", # Added class to add padding
               h3("Employment Package"),
               h6("Click on a document thumbnail below to view it:"),
               div(
                 class = "doc-container",
                 
                 # Cover Letter
                 div(
                   actionLink("view_coverletter", div(
                     tags$img(src = "cover_letter_thumbnail.png", width = "90px", height = "120px"), 
                     tags$p("Cover Letter", style = "margin-top: 5px; text-indent: 0; text-align: center;")
                   ))
                 ),
                 
                 # Resume
                 div(
                   actionLink("view_resume", div(
                     tags$img(src = "resume_thumbnail.png", width = "90px", height = "120px"),
                     tags$p("Resume", style = "margin-top: 5px; text-indent: 0; text-align: center;")
                   ))
                 ),
                 
                 # References
                 div(
                   actionLink("view_references", div(
                     tags$img(src = "references_thumbnail.png", width = "90px", height = "120px"),
                     tags$p("References", style = "margin-top: 5px; text-indent: 0; text-align: center;")
                   ))
                 )
               )
             )
           ),
           
           "document_viewer" = uiOutput("documentViewer"),
           
           "portfolio" = tagList(
             h3("Work Portfolio"),
             h6("Explore my work by selecting one of the sections below."),
             
             # Subtabs
             tabsetPanel(
               id = "portfolio_subtabs", # ID to control the active subtab dynamically
               tabPanel(
                 "Research Papers", 
                 value = "research", 
                 h4("Research Papers"),
                 
                 # 21st Century Climate in New Zealand
                 div(
                   style = "margin-bottom: 20px;",
                   h5("21st Century Climate in New Zealand"),
                   p("This research explores climate projections for New Zealand in the 21st century, focusing on temperature and precipitation patterns under various emission scenarios."),
                   div(
                     style = "text-align: center;",
                     actionLink("view_ccnz", div(
                       tags$img(src = "CCNZ_thumbnail.png", width = "100px", height = "150px"),
                       tags$p("21st Century Climate in New Zealand", style = "margin-top: 5px; text-align: center;")
                     ))
                   )
                 ),
                 
                 # Mount Shasta
                 div(
                   style = "margin-bottom: 20px;",
                   h5("Mount Shasta"),
                   p("This paper investigates the glacial retreat on Mount Shasta and its implications for water resources and ecosystems."),
                   div(
                     style = "text-align: center;",
                     actionLink("view_shasta", div(
                       tags$img(src = "MtShasta_thumbnail.png", width = "100px", height = "150px"),
                       tags$p("Mount Shasta", style = "margin-top: 5px; text-align: center;")
                     ))
                   )
                 )
               ),
               tabPanel("Data Analysis Projects", value = "analysis", h4("Data Analysis Projects"), p("Content for data analysis projects here...")),
               tabPanel("Static Maps", value = "static", h4("Static Maps"), p("Content for static maps here...")),
               tabPanel("Interactive Maps", value = "interactive", h4("Interactive Maps"), p("Content for interactive maps here..."))
             )
           ),
           
           
           
           
           "informational_interview" = tagList(
             div(class = "custom-paragraph",
             h3("Informational Interview"),
             p("I interviewed Mandy and Bobby.  We all applied to similar positions, which all included varying realms of GIS.  It was insightful to hear the different ways we all described different projects we have worked on for school.  I went last so I was able to absorb both of their approaches at being interviewed.  One challenge for myself is the amount of filler words, I found myself counting the number of times I said “umm” and “ahh”.  The most valuable takeaway for me was the questions asked by the interviewer.  I could only prepare so much so being able to clearly identify my strengths of describing a specific project and talking about GIS was great.  Knowing that I can handle those questions provides me room for growth and preparation in other areas.  Areas like outside of the technical realm, what are my skills and weaknesses.  Being able to describe soft skills and interpersonal skills seems like it would be really valuable, the mock interview provided that insight.  I found it surprising that I was slightly nervous even though there was nothing on the line at all.  I mentioned in the previous write up that maybe it was because of the way I was dressed, I felt more casual than I should have.  The old ideology of “dress for success” seems to influence my comfort zone so I will be dressing professionally for any interviews as well."), 
             p("The mock interview definitely made me recognize the importance of knowing key technical terms, knowing the language of the GIS world.  Being able to do the work is one thing, but being able to talk about and discuss it in detail is another.  Knowing this will help me focus on learning more terms to be better prepared for an interview with people who have been doing GIS for a couple decades.  On the other hand, I learned that I will need to be able to describe my technical skills to people who may not have any background in the field.  All of it gave me a better understanding on how to be prepared for an actual interview, which I have in January.  Figuring out who the interview will be conducted with is critical, and while not always possible, I believe that researching that information will provide me valuable insights to the company.  I believe the exercise made me recognize the importance of researching the companies I have applied for, and get to know as much as I can about the values and scope of work.  Researching companies and the people that work there is really easy to do on LinkedIn, so that is my plan of action going forward.  The main takeaway from the exercise is that it is better to be as prepared as possible for any interviews, which lines up with the fact that I’d rather have it and not need it, than need it and not have it.")
             )
             ),
           
           "linkedin" = tagList(
             div(class = "main-panel-content",
             h3("LinkedIn Profile"),
             h6(
               "LinkedIn link: ",
               a(href = "https://www.linkedin.com/in/mike-peckham-kuruka?trk=profile-badge", 
                 "Michael Peckham", 
                 target = "_blank")
             ),
             tags$img(src = "header.png", width = "100%"),
             tags$img(src = "about.png", width = "100%"),
             tags$img(src = "Experience1.png", width = "100%"),
             tags$img(src = "Experience2.png", width = "100%"),
             tags$img(src = "Experience3.png", width = "100%"),
             tags$img(src = "Education.png", width = "100%")
             )
           ),
           
           
           "ai_social_media" = h3("AI & Social Media Content Goes Here"),
           "career_pathways_grad_school" = h3("Career Pathways & Grad School Content Goes Here"),
           "action_plan" = h3("Action Plan Content Goes Here"),
           h3("Select a tab to view content")
    )
    }
  })
}

shinyApp(ui, server)
