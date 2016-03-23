%% file ETHbrief.cls

%% --- Identification part ---
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ColumbiaAssm}[2004/10/15 v1.0 ETH Uebung]

%% --- Declaration of options ---
%%\DeclareOption*{\PassOptionsToClass{\CurrentOption}{letter}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

%% --- Execution of options ---
\ProcessOptions

%% --- Package loading ---
%\RequirePackage{ETHlogo}
\RequirePackage[latin1]{inputenc}

%% --- Special commands for package and class files ---

%% --- Special class file commands ---
%%\LoadClass[a4paper]{letter}
\LoadClass[letter,11pt]{exam}

%% --- Defining the (sub-)class ---
\setlength{\textwidth}{16.5cm}
\setlength{\marginparwidth}{1.5cm}
%\setlength{\marginparwidth}{4cm}
%\setlength{\marginparwidth}{0cm}
\setlength{\parindent}{0cm}
\setlength{\parskip}{0.15cm}
\setlength{\textheight}{23cm}
\setlength{\oddsidemargin}{0cm}
\setlength{\evensidemargin}{\oddsidemargin}
\setlength{\topmargin}{0cm}
\setlength{\headheight}{0cm}
\setlength{\headsep}{0cm}

\renewcommand{\familydefault}{\sfdefault}

% text parameters
\newcommand{\EinheitNameText}{}
\newcommand{\EinheitInfoText}{}
\newcommand{\PersonNameText}{}
\newcommand{\PersonAdresseText}{}
\newcommand{\PersonInfoText}{}
\newcommand{\AssistantNameOneText}{}
\newcommand{\AssistantNameTwoText}{}
\newcommand{\AssistantOneAdresseText}{}
\newcommand{\AssistantTwoAdresseText}{}
\newcommand{\OrtText}{New York City}
\newcommand{\BetreffText}{}
\newcommand{\DueDateText}{}
\newcommand{\KurstitelText}{}
\newcommand{\CoursePageText}{}
\newcommand{\SemesterText}{}

\newcommand{\EinheitName}  [1]{\renewcommand{\EinheitNameText}{#1}}
\newcommand{\EinheitInfo}  [1]{\renewcommand{\EinheitInfoText}{#1}}
\newcommand{\PersonName}   [1]{\renewcommand{\PersonNameText}{#1}}
\newcommand{\PersonAdresse}[1]{\renewcommand{\PersonAdresseText}{#1}}
\newcommand{\PersonInfo}   [1]{\renewcommand{\PersonInfoText}{#1}}
\newcommand{\AssistantNameOne} [1]{\renewcommand{\AssistantNameOneText}{#1}}
\newcommand{\AssistantOneAdresse}[1]{\renewcommand{\AssistantOneAdresseText}{#1}}
\newcommand{\AssistantNameTwo} [1]{\renewcommand{\AssistantNameTwoText}{#1}}
\newcommand{\AssistantTwoAdresse}[1]{\renewcommand{\AssistantTwoAdresseText}{#1}}
\newcommand{\Ort}          [1]{\renewcommand{\OrtText}{#1}}
\newcommand{\Betreff}      [1]{\renewcommand{\BetreffText}{#1}}
\newcommand{\Kurstitel}    [1]{\renewcommand{\KurstitelText}{#1}}
\newcommand{\CoursePage} [1]{\renewcommand{\CoursePageText}{#1}}
\newcommand{\Semester}    [1]{\renewcommand{\SemesterText}{#1}}
\newcommand{\DueDate}          [1]{\renewcommand{\DueDateText}{#1}}

% English commands as synonyms (Fritz Zaucker, 2001-01-29)
\let\UnitName\EinheitName
\let\UnitInfo\EinheitInfo
\let\PersonAddress\PersonAdresse
\let\City\Ort
\let\Subject\Betreff
\let\subject\Betreff


\newcommand{\AddSubmissionRules}[0]{
Students are encouraged to work together, but homework write-ups must be done individually and must be entirely the author's own work.  Homework is due at the {\bf beginning} of the class for which it is due. {\bf Late homework will not be accepted under any circumstances. } To receive full credit, students must thoroughly explain how they arrived at their solutions and include the following information on their homeworks: name, UNI, homework number (e.g., HW03), and class (STAT W4400).  All homework must be turned in online through Courseworks in PDF format, have a .pdf extension (not zip or other archive!), and be less than 4MB.  If programming is part of the assignment, the code must be turned in in one or more .R files.  Homeworks not adhering to these requirements will receive no credit.  For your convenience (not required), a tex template for producing nice PDF files can be found on courseworks.}

\newcommand{\makeAssmHeader}[4]{%
  \thispagestyle{empty} % no headers and footers (i.e. page numbers)
                        % on the first page
  \setlength{\unitlength}{1mm}
  \begin{picture}(165,40)
    \put(123,40){%
      \begin{minipage}[t]{90mm}
        %\rule{90mm}{0mm}\\
        %% \textbf{\EinheitNameText}\vspace{1mm}\\
        %% \EinheitInfoText
        %% \vspace{1mm} \\
        \textbf{\small\PersonNameText}\\
        {\small\PersonAdresseText}
        \vspace{1mm}\\ 
        %% {\small\PersonInfoText}
        %% \vspace{1mm} \\
        \textbf{\small\AssistantNameOneText}\\
        {\small\AssistantOneAdresseText}
        \vspace{1mm}\\
        \textbf{\small\AssistantNameTwoText}\\
        {\small\AssistantTwoAdresseText}\\
	\vspace{6mm} \\
	%% \begin{tabular}{@{}l@{~}l@{}}
        %% \textbf{\small Email questions to:} & \textbf{\small #3} \\
	%% & {\small #4} \\
    	%% \end{tabular}
      \end{minipage}
      }
    \put(0,42.5){%
      \begin{minipage}[t]{98mm}
        \rule{98mm}{0mm}\\
        \raggedright \textbf{\large\KurstitelText}\\ { \SemesterText} \par {\CoursePageText } \par
      \end{minipage}
      }
    \put(0,10.5){%
      \begin{minipage}[t]{250mm}
        \textbf{{\Large Homework #1} \\[1.0ex] }\\%(#2)}
        {#2}\\
      \end{minipage}
    }
  \end{picture}
  \par
}

\newcommand{\makeSolHeader}[4]{%
  \thispagestyle{empty} % no headers and footers (i.e. page numbers)
                        % on the first page
  \setlength{\unitlength}{1mm}
  \begin{picture}(165,40)
    \put(123,40){%
      \begin{minipage}[t]{90mm}
        %\rule{90mm}{0mm}\\
        %% \textbf{\EinheitNameText}\vspace{1mm}\\
        %% \EinheitInfoText
        %% \vspace{1mm} \\
        \textbf{\small\PersonNameText}\\
        {\small\PersonAdresseText}
        \vspace{1mm}\\ 
        %% {\small\PersonInfoText}
        %% \vspace{1mm} \\
        \textbf{\small\AssistantNameOneText}\\
        {\small\AssistantOneAdresseText}
        \vspace{1mm}\\
        \textbf{\small\AssistantNameTwoText}\\
        {\small\AssistantTwoAdresseText}\\
	\vspace{6mm} \\
	%% \begin{tabular}{@{}l@{~}l@{}}
        %% \textbf{\small Email questions to:} & \textbf{\small #3} \\
	%% & {\small #4} \\
    	%% \end{tabular}
      \end{minipage}
      }
    \put(0,42.5){%
      \begin{minipage}[t]{98mm}
        \rule{98mm}{0mm}\\
        \raggedright \textbf{\large\KurstitelText}\\ { \SemesterText} \par {\CoursePageText } \par
      \end{minipage}
      }
    \put(0,16.5){%
      \begin{minipage}[t]{250mm}
        \textbf{{\Large Solutions: Assignment #1} \\[1.0ex] }\\%(#2)}
      \end{minipage}
    }
  \end{picture}
  \par
}


\newcommand{\makeGenericHeader}[4]{%
  \thispagestyle{empty} % no headers and footers (i.e. page numbers)
                        % on the first page
  \setlength{\unitlength}{1mm}
  \begin{picture}(165,40)
    \put(123,40){%
      \begin{minipage}[t]{90mm}
        %\rule{90mm}{0mm}\\
        %% \textbf{\EinheitNameText}\vspace{1mm}\\
        %% \EinheitInfoText
        %% \vspace{1mm} \\
        \textbf{\small\PersonNameText}\\
        {\small\PersonAdresseText}
        \vspace{1mm}\\ 
        %% {\small\PersonInfoText}
        %% \vspace{1mm} \\
        \textbf{\small\AssistantNameOneText}\\
        {\small\AssistantOneAdresseText}
        \vspace{1mm}\\
        \textbf{\small\AssistantNameTwoText}\\
        {\small\AssistantTwoAdresseText}\\
	\vspace{6mm} \\
	%% \begin{tabular}{@{}l@{~}l@{}}
        %% \textbf{\small Email questions to:} & \textbf{\small #3} \\
	%% & {\small #4} \\
    	%% \end{tabular}
      \end{minipage}
      }
    \put(0,42.5){%
      \begin{minipage}[t]{98mm}
        \rule{98mm}{0mm}\\
        \raggedright \textbf{\large\KurstitelText}\\ { \SemesterText} \par {\CoursePageText } \par
      \end{minipage}
      }
    \put(0,10.5){%
      \begin{minipage}[t]{250mm}
        \textbf{{\Large #1} \\[1.0ex] }\\%(#2)}
        {#2}\\
      \end{minipage}
    }
  \end{picture}
  \par
}

