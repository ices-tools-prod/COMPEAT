# Docker S3 Configuration for COMPEAT

## Problem
The COMPEAT application requires S3 credentials and configuration to download and upload assessment files to MinIO. Without these environment variables, the application fails with `SignatureDoesNotMatch` errors because it attempts to sign requests without valid credentials.

## Required Environment Variables

To use S3 with COMPEAT in Docker, you must pass these environment variables:

```
AWS_ACCESS_KEY_ID=your-minio-access-key
AWS_SECRET_ACCESS_KEY=your-minio-secret-key
AWS_S3_ENDPOINT=minio.dive.edito.eu
AWS_DEFAULT_REGION=us-east-1          # Can be any value for MinIO
COMPEAT_S3_BUCKET=compeat             # Your bucket name
COMPEAT_S3_PREFIX=compeat             # Optional: prefix for S3 objects (default: "compeat")
COMPEAT_S3_URL_STYLE=path             # Optional: "path" or "virtual" (default: "path")
```

## Setup Methods

### Method 1: Using Docker Environment Variables (Recommended)

Stop the current container and start a new one with environment variables:

```bash
docker stop compeat_container
docker rm compeat_container

docker run -d \
  --name compeat_container \
  -p 81:3838 \
  -e AWS_ACCESS_KEY_ID="your-access-key" \
  -e AWS_SECRET_ACCESS_KEY="your-secret-key" \
  -e AWS_S3_ENDPOINT="minio.dive.edito.eu" \
  -e AWS_DEFAULT_REGION="us-east-1" \
  -e COMPEAT_S3_BUCKET="compeat" \
  -e COMPEAT_S3_PREFIX="compeat" \
  compeat:multistage
```

### Method 2: Using an .env File

Create a file named `.env` in your COMPEAT directory:

```bash
# .env
AWS_ACCESS_KEY_ID=your-access-key
AWS_SECRET_ACCESS_KEY=your-secret-key
AWS_S3_ENDPOINT=minio.dive.edito.eu
AWS_DEFAULT_REGION=us-east-1
COMPEAT_S3_BUCKET=compeat
COMPEAT_S3_PREFIX=compeat
COMPEAT_S3_URL_STYLE=path
```

Then run Docker with the env file:

```bash
docker stop compeat_container
docker rm compeat_container

docker run -d \
  --name compeat_container \
  -p 81:3838 \
  --env-file .env \
  compeat:multistage
```

### Method 3: Using Docker Compose

Create a `docker-compose.yml` file:

```yaml
version: '3.8'

services:
  compeat:
    image: compeat:multistage
    container_name: compeat_container
    ports:
      - "81:3838"
    environment:
      AWS_ACCESS_KEY_ID: ${AWS_ACCESS_KEY_ID}
      AWS_SECRET_ACCESS_KEY: ${AWS_SECRET_ACCESS_KEY}
      AWS_S3_ENDPOINT: ${AWS_S3_ENDPOINT}
      AWS_DEFAULT_REGION: ${AWS_DEFAULT_REGION}
      COMPEAT_S3_BUCKET: ${COMPEAT_S3_BUCKET}
      COMPEAT_S3_PREFIX: ${COMPEAT_S3_PREFIX}
      COMPEAT_S3_URL_STYLE: ${COMPEAT_S3_URL_STYLE}
```

Then run:

```bash
docker-compose up -d
```

## Verification

After starting the container with S3 credentials, verify they're loaded:

```bash
docker exec compeat_container env | grep AWS_
docker exec compeat_container env | grep COMPEAT_
```

You should see output like:
```
AWS_ACCESS_KEY_ID=your-access-key
AWS_SECRET_ACCESS_KEY=***secret***
AWS_S3_ENDPOINT=minio.dive.edito.eu
...
```

## Testing S3 Connection

After restarting the container with credentials, try running an assessment:

1. Open COMPEAT at http://localhost:81
2. Select an assessment period (e.g., "COMP 5 (2021-2026)")
3. The assessment should prepare successfully
4. Wait for completion
5. Check MinIO to verify files were uploaded

Expected files in MinIO:
- `compeat/COMP 5 (2021-2026)/current.json` - Pointer to latest run
- `compeat/COMP 5 (2021-2026)/runs/<run-id>/manifest.json` - File manifest
- `compeat/COMP 5 (2021-2026)/runs/<run-id>/output/*` - Assessment output files

## Troubleshooting

### Still getting SignatureDoesNotMatch?
1. Verify credentials are correct in MinIO
2. Check AWS_S3_ENDPOINT is correct and accessible
3. Run `docker exec compeat_container env` to confirm variables are set
4. Check container logs: `docker logs compeat_container`

### Error: "Could not create assessment output directory"?
Check that the container has write permissions to `/srv/shiny-server/data/`

### Local storage mode instead of S3?
If `AWS_S3_ENDPOINT` is not set, COMPEAT defaults to local storage. Files will be stored in `/srv/shiny-server/data/` instead of S3.

## Security Note

- Store credentials securely (use Docker secrets, environment variable managers, or credential stores)
- Never commit `.env` files or credentials to git
- Use container-specific credentials with limited S3 permissions
- For production, consider using IAM roles or temporary credentials
