#!/bin/bash

DATABASE_URL="postgres://linguine:linguine@127.0.0.1:5432/?sslmode=disable" dbmate migrate
