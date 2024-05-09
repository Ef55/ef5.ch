---
title: "Curriculum vitae"
enable_git: false
---

# Education

- **EPFL** (Lausanne, Switzerland) <span align="right"> ggg. </span>
    - Master in Computer Science
    - Bachelor in Computer Science

# Experience

<!-- \begin{timeline}
	\item \textbf{EPFL (Swiss Federal Institute of Technology)}
	\begin{itemize}
		\period{2021-2024}{Master in Computer Science}
		\begin{multicols}{2}
			\begin{itemize}
				\item Average: 5.75/6
				\item Specialization: Foundations of Software
				\item Rank: 12/157
			\end{itemize}
		\end{multicols}
		\period{2018-2021}{Bachelor in Computer Science}
		\begin{multicols}{2}
			\begin{itemize}
				\item Average: 5.51/6
				\item Rank: 7/109
			\end{itemize}
		\end{multicols}
		% \item \textbf{Relevant coursework}:
		% \begin{multicols}{2}
		% 	\begin{itemize}
		% 		\item Computer language processing
		% 		\item Foundations of software
		% 		\item Advanced compiler construction
		% 		\item Formal verification
		% 	\end{itemize}
		% \end{multicols}
	\end{itemize}
	
	% \period{2015-2018}{Maturité gymnasiale --- Lycée Cantonal de Porrentruy}
	% \begin{itemize}[itemsep=-0.5em]
	% 	\item Specific option: Physics and Applied Mathematics
	% 	\item Secondary option: Computer Science
	% \end{itemize}
\end{timeline}

\section{Experience}
\begin{timeline}
	\period{March 2024 - Present}{``Master's valorisation'' --- Systems and Formalisms Lab, EPFL}

	\period{March-August 2023}{Student intern --- Oracle Labs}
	Supervised by Dr.\ Peter Hofer.\\
	Implemented an almost complete support for the new upcoming foreign functions interface of the Java ecosystem in native-image, an ahead-of-time compiler for Java, and SubstrateVM, its companion runtime. The implementation fully supports dynamic library loading, calls from Java to native functions, and calls from native to Java methods.

	\period{July-September 2022}{Research intern --- Laboratory for Automated Reasoning and Analysis, EPFL}
	% Worked on the theory behind ghost code and its elimination. 
	% This research project was later continued as one of my Master projects.

	\period{2020-2023}{Student assistant --- EPFL}
	For the following classes:
	\begin{multicols}{2}
		\begin{citemize}
			\item Formal Verification (Fall 2022, Fall 2023)
			\item Computer Language Processing (Fall 2021)
			% \begin{itemize}
			% 	\item Guided students through the creation of a compiler in Scala;
			% 	\item Presented the project to the students;
			% 	\item Helped the course team improve the course material and infrastructure.
			% \end{itemize}

			% \begin{itemize}
			% 	\item Helped students deepen their understanding of object oriented programming;
			% 	% \item Graded the students' projects.
			% \end{itemize}
		\end{citemize}
	\end{multicols}
	\begin{citemize}
		\item Practice of object-oriented programming (Spring 2020, Spring 2021)
	\end{citemize}
	Responsibilities included: presenting labs to students, correcting labs and exams, creating and maintaining grading infrastructures, overhauling labs, writing solution sheets.
	% \begin{multicols}{2}
	% 	\begin{citemize}
	% 		\item Presenting labs to students
	% 		\item Correcting labs and exams
	% 		\item Creating and maintaining infrastructures
	% 		\item Overhauling labs
	% 		\item Writing solutions sheets
	% 	\end{citemize}
	% \end{multicols}

	
	%\period{August 2019}{Developer -- NextDay.Vision SA}
	% \begin{itemize}
	% 	\item Developed a control interface in ASP.net for RADIUS servers.
	% \end{itemize}
\end{timeline}


\section{Projects}
\begin{itemize}
	\project[true]{Fall 2023}{Mechanization of ECMAScript's regexes semantics}
	{
		Master thesis done at \emph{Systems and Formalisms lab, EPFL} (Prof.\ Clément Pit-Claudel).\\
		Ported the JavaScript regexes specification to the Coq proof assistant.
		Care was taken to ensure that the ported specification was faithful to the original paper one, and that one could convince oneself of this fact.
		Proved key properties of the specification, such as termination.
		Additionally dis-proved some incorrect simplifications found in the literature about these semantics.
	}

	\project[true]{Summer 2022}{Ghost code, erasability and non-interference}
	{
		Internship done at \emph{Laboratory for Automated Reasoning and Analysis, EPFL} (Prof.\ Viktor Kunčak).\hfill{}\presentation{https://github.com/Ef55/Reports-and-presentations/blob/main/ghost_code/build/presentation.pdf}\\
		Developed a calculus allowing the elimination of pieces of code (called ``ghost'') without altering the result or visible effects of a computation. The calculus notably supports both mutable references and subtyping. Also investigated the relation between ghost code and the non-interference property, which comes from the domain of security type systems.
	}

	\project[true]{Spring 2022}{Computation expressions for Scala 3}
	{
		Semester project done at \emph{Programming Methods Laboratory, EPFL} (Prof.\ Martin Odersky).\hfill{}\repository{https://github.com/Ef55/scala-expression-processor}\\
		Implemented an adaptation of F\#'s computation expressions in Scala 3 using its meta-programming facilities. Demonstrated how it could be used to make more natural and practical domain specific languages.
	}

	\project[true]{Fall 2021}{Verified System F in Stainless}{
		Course project for the \emph{Formal verification} class (Prof.\ Viktor Kunčak).\hfill{}\repository{https://github.com/Ef55/stainless-stlc/tree/system-f}\\
		Implemented an evaluator for System F in the subset of Scala supported by Stainless, a verification tool for Scala.
		Then proved the main properties of the implemented calculus, 
		most notably that it satisfies progress and preservation.
		This project used to be the second largest one (in number of verification conditions) done in Stainless. It is also still used as a benchmark for Stainless.
	}

	\project[false]{Spring 2021}{Bachelor project}{Making syntaxes LL1 through transformations \repository{https://github.com/Ef55/bscp-syntax-transformation}}{
		Done at \emph{Laboratory for Automated Reasoning and Analysis, EPFL}.\\
		Implemented transformations on syntaxes in a LL1 parsing library (ScaLL1on) 
		to allow more syntaxes to be parsed in linear time.
		Formalized and proved some properties of the transformations,
		in particular that the set of parsed strings was left unchanged.
	}

	% \project[false]{Summer 2021}{A Terrible Front-end Library \repository{https://github.com/Ef55/tfl/}}{
	% 	Designed and implemented a parsing library in C++.
	% 	The project was originally an experiment on how concisely
	% 	a lexer and parser could be implemented.
	% 	It later evolved into a parsing library, 
	% 	focused on ease of use, 
	% 	by providing a parser combinator interface and being value-aware.
	% }
\end{itemize} -->