FROM node:20-alpine AS build
WORKDIR /app
COPY devops-ui/package*.json ./
RUN npm install
COPY devops-ui/ .
RUN rm -rf node_modules/.cache
RUN npm run build

FROM nginx:1.25-alpine
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY --from=build /app/dist /usr/share/nginx/html
EXPOSE 80
