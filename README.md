# conditional-independency-of-US-stocks-given-US-dollar-index
The main objective is to find out how federal reserve interest rates affect the behavior of the stock market. i.e Conditional Independency of different stocks given US dollar and pairwise correlation of each stocks together.
We Sliced the data on the federal reserve based on the increase, decrease and stability of interest rates and plotted the graphs based on conditional independence results. How do the stocks differ?
We are focusing on different sectors of stocks such as energy, technology, consumer staple, finance and healthcare. These have high correlation with US dollars, If we condition on US dollar we expect the dependency to diminish.
We used python for data collection and preperation. We used the Yahoo finanace library to download historical stock data for a list of tickers and conducted some basic data analysis.
We used R programming to conduct the conditional independency by importing the bnlearn library which contains the ci.test() function.
