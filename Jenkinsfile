/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
*/

pipeline {
  agent any
  stages {
    stage('Build') {
      agent {
        docker {
          image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
        }
      }
      steps {
        sh 'jenkins_scripts/build.sh'
        stash name: 'snapshots-to-publish' includes: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
      }
    }
    stage('Upload Snapshots') {
      /* This must run on michalis.ii.uni.wroc.pl, outside Docker,
         since it directly copies the files. */
      agent { label 'web-michalis-ii-uni-wroc-pl' }
      steps {
        unstash name: 'snapshots-to-publish'
        sh 'jenkins_scripts/upload_snapshots.sh'
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
    }
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
