FROM r-base:4.4.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-ggplot2 \
    r-cran-tidyr \
    r-cran-reshape2 \
    r-cran-car \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY brexit_data.csv /app/brexit_data.csv
COPY analysis.r /app/analysis.r

CMD ["Rscript", "analysis.r"]
