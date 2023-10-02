# Docker Containers
The com and org websites are deployed via Docker containers.
There are three containers involved:
- ddh-app-base - A base container that is manually built when external dependencies are changed
- ddh-app-org - The org container that extends ddh-app-base
- ddh-app-com - The com container that extends ddh-app-base

## Manually Building ddh-app-base
Instructions for building and pushing to the ddh-app-base container are in the aws console.
Search for ECR, Click `Elastic Container Registry`, Select the `ddh-app-base`, Click `View push commands`.
The only change from those instructions is in the `docker build` command:
```
docker build -t ddh-app-base -f Dockerfile.base .
```
On Mac M1/M2/etc you will need to specify the platform:
```
docker build -t ddh-app-base -f Dockerfile.base --platform=linux/amd64 .
```

## Building ddh-app-org
A pipeline for ddh-app-org should automatically build when there are changes to the main branch.
If for some reason you can just follow the AWS console instructions for building this container.
The only adjustment in this case would be to ad `--platform=linux/amd64` to build if on Mac M1/M2/tec.

## Building ddh-app-com
A pipeline for ddh-app-com should automatically build when there are changes to the main branch.
If for some reason you can just follow the AWS console instructions for building this container.
The only adjustment in this case would be to ad `--platform=linux/amd64` to build if on Mac M1/M2/tec.
