DBURL="postgres://api:passwd@localhost:5432/hello-graphql?sslmode=disable"


# migrate postgresql database
.PHONY: migrate-up
migrate-up:
	migrate -path migrations/ -database $(DBURL) up
	./scripts/genTables.sh

# migrate down postgresql database
.PHONY: migrate-down
migrate-down:
	migrate -path migrations/ -database $(DBURL) down
	./scripts/genTables.sh

migrate-down-hard:
	psql $(DBURL) -Atc "drop schema public cascade; create schema public;"
