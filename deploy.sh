#!/bin/bash
# Deploys the application to the $DEPLOY_DIR with a script to run it

DEPLOY_DIR='./install'  # The directory to deploy the ready app to
ASSEMBLY_DIR='./cli/target/scala-2.10'
ASSEMBLY_FILE='scalaxb-app.jar'
LAUNCH_FILE='scalaxb'

# Build the app
sbt app/assembly

# Prepare the deployment directory
if [ ! -d "$DEPLOY_DIR" ]; then mkdir $DEPLOY_DIR; fi
rm $DEPLOY_DIR/* 2> /dev/null  # Redirect the error output stream to sink

# Move the app jar to the directory
mv "$ASSEMBLY_DIR/$ASSEMBLY_FILE" $DEPLOY_DIR

# Create the launch script for the app
echo -e '#!/bin/bash\njava -jar '$ASSEMBLY_FILE' $@' > "$DEPLOY_DIR/$LAUNCH_FILE"
chmod +x "$DEPLOY_DIR/$LAUNCH_FILE"