FROM lmassa/covid19-model-base:0.5
ARG BUILDHOST

# Run the image as a non-root user
RUN adduser --disabled-password --gecos "" myuser

# Add our code
WORKDIR /opt/webapp
COPY webapp/assets /opt/webapp/assets
COPY queue_model /opt/webapp/queue_model
COPY ["1 - Model", "/opt/webapp/1 - Model"]
COPY ["0 - Parameters", "/opt/webapp/0 - Parameters"]

COPY web.py /opt/webapp/web.py
COPY webapp /opt/webapp/webapp


RUN echo "Built at $(date) on $BUILDHOST" > /buildinfo
USER myuser
# Run the app.  CMD is required to run on Heroku
# $PORT is set by Heroku			

CMD gunicorn --bind 0.0.0.0:$PORT web:server
