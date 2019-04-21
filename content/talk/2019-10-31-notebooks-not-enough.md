+++
date = 2017-01-01T00:00:00  # Schedule page publish date.

title = "When Notebooks are not Enough: Constructing Workflows for Reproducible Analytics"
time_start = 2018-10-31T16:00:00
time_end = 2018-10-31T17:00:00
abstract = "While computational notebooks offer scientists and engineers many helpful features, the limitations of this medium make it but a starting point in creating software - the practical goal of data science. Where do we go from computational notebooks if our projects require multiple interconnected scripts and dynamic documents? How do we ensure reproducibility amidst growing complexity of analyses and operations?" 
abstract_short = "Basic techniques for designing reproducible workflows are demonstrated and discussed"  
event = "Matrix Institute Colloquium"
event_url = "https://example.org"
location = "University of Victoria"

# Is this a selected talk? (true/false)
selected = false

# Projects (optional).
#   Associate this talk with one or more of your projects.
#   Simply enter the filename (excluding '.md') of your project file in `content/project/`.
projects = ["data-science-studio"]

# Links (optional).
url_pdf = "https://drive.google.com/open?id=1_HGJC0RxfKqjkNgNhN7h8X_y5c9WdXft"
url_slides = ""
url_video = ""
url_code = ""

# Does the content use math formatting?
math = true

# Does the content use source code highlighting?
highlight = true

# Featured image
# Place your image in the `static/img/` folder and reference its filename below, e.g. `image = "example.jpg"`.
[header]
image = "headers/bubbles-wide.jpg"
caption = "My caption :smile:"



+++

I will use a concrete analytical example to demonstrate how constructing workflows for
reproducible analyses can serve as the next step from computational notebooks towards
creating an analytical software. First, I will demonstrate a reproducible graphing system
designed for the IPDLN-2018 hackathon , organized by Statistics Canada. The system evaluates synthetic socioeconomic and mortality data with logistic regression. Then I will discuss the workflow of the project that implements this graphing system
( github.com/andkov/ipdln-2018-hackathon ) and the RStudio + GitHub setup that hosts it. I will conclude by building the case to prefer reproducible workflows with version control over computational notebooks (e.g. Jupyter, R Notebook).

Embed your slides or video here using [shortcodes](https://sourcethemes.com/academic/post/writing-markdown-latex/). Further details can easily be added using *Markdown* and $\rm \LaTeX$ math code.
