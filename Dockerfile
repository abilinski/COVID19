FROM lmassa/covid19-model-base:0.3
ARG BUILDHOST

# Install python and pip
RUN apt update
RUN apt-get install python3 python3-pip bash -y
ADD ./requirements.txt /tmp/requirements.txt

# Install dependencies
RUN pip3 install --no-cache-dir -q -r /tmp/requirements.txt


# Run the image as a non-root user
RUN adduser --disabled-password --gecos "" myuser


# Add our code
WORKDIR /opt/webapp
ADD . /opt/webapp/


RUN echo "Built at $(date) on $BUILDHOST" > /buildinfo
USER myuser
# Run the app.  CMD is required to run on Heroku
# $PORT is set by Heroku			
CMD gunicorn --bind 0.0.0.0:$PORT web:server
