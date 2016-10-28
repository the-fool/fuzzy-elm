FROM node:6
EXPOSE 3000
WORKDIR /app
ADD entry.sh /app
RUN chmod a+x /app/entry.sh
ENTRYPOINT ["/app/entry.sh"]
