FROM lmassa/covid19-model-base:0.3
ARG BUILDHOST

# Install python and pip
RUN apt update
RUN apt-get install python3 python3-pip bash -y
COPY ./requirements.txt /tmp/requirements.txt

# Install dependencies
RUN pip3 install --no-cache-dir -r /tmp/requirements.txt


# Run the image as a non-root user
RUN adduser --disabled-password --gecos "" myuser


# Add our code
WORKDIR /opt/webapp
COPY assets /opt/webapp/assets
COPY ["1 - Model", "/opt/webapp/1 - Model"]
COPY ["0 - Parameters", "/opt/webapp/0 - Parameters"]

COPY web.py /opt/webapp/web.py
COPY webapp /opt/webapp/webapp


RUN echo "Built at $(date) on $BUILDHOST" > /buildinfo
USER myuser
# Run the app.  CMD is required to run on Heroku
# $PORT is set by Heroku			

CMD gunicorn --bind 0.0.0.0:$PORT web:server
