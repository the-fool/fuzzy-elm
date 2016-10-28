FROM node:6

WORKDIR /app

COPY webpack.config.js package.json elm-package.json /app/

COPY entry.sh /app
RUN chmod a+x /app/entry.sh

ENTRYPOINT ["/app/entry.sh"]
