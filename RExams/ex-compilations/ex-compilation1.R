library(exams)



ex_demo <-
  c(
    "switzerland.Rmd",  # Multiple Choice, statisch
    "swisscapital.Rmd",  # Single Choice, dynamisch
    "countrycodes1.Rmd",  # String, dynamisch
    "Laender1.Rmd",  # String, dynamisch
    "Numerisch1.Rmd",  # Numerisch, statisch
    "Numerisch2.Rmd",  # Numerisch, dynamisch
    "Numerisch3.Rmd",  # Numerisch, dynamisch
    "Logik3.Rmd"  # Single Choice, dynamisch
  )



exams2html(file = ex_demo,
           n = 2,
           dir = "output",
           name = "Exam-Test",
           edir = "exs",
           mathjax = TRUE,  # für Chrome nötig
           question = "Aufgabe",
           solution = "Lösung")



exams2moodle(file = ex_demo,
             , name = "Testexamen-RExams"
             , dir = "output"
             , iname = TRUE,
             , testid = TRUE
             , n = 10,
             , edir = "exs"
             , verbose = TRUE
             , quiet = FALSE
             , rule = "none"  # for partial credit scoring
             , mchoice = list(shuffle = TRUE,
                              eval = exams_eval(rule = "none"))
             , schoice = list(shuffle = TRUE,
                              eval = exams_eval(rule = "none")))





exams2moodle(file = numerisch3,
             , name = "Testexamen-RExams"
             , iname = TRUE
             , dir = "output"
             , n = 10,
             , edir = "exs"
             , verbose = TRUE
             , quiet = FALSE
             , rule = "none"
             , mchoice = list(shuffle = TRUE,
                              eval = exams_eval(rule = "none"))
             , schoice = list(shuffle = TRUE,
                              eval = exams_eval(rule = "none")))