# PostgreSQL Docker Connection Troubleshooting

## Problem
pgAdmin fails to connect with "password authentication failed" even with correct credentials.

## Cause
Local PostgreSQL installations (Windows services) conflict with Docker by using the same port (5432).

## Fix

### 1. Stop local PostgreSQL services
```cmd
net stop postgresql-x64-13
net stop postgresql-x64-14
```

### 2. Verify only Docker is using port 5432
```cmd
netstat -ano | findstr :5432
```
Should show only one process (Docker).

### 3. If volume has old credentials, reset it
```cmd
docker compose down -v
docker compose up -d
```

## Prevention
Consider disabling automatic startup for local PostgreSQL services if you primarily use Docker:
1. Open `services.msc`
2. Find `postgresql-x64-XX` services
3. Right-click → Properties → Set Startup type to **Manual**