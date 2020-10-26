/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
*/

pipeline {
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_update_docker_image/master', threshold: hudson.model.Result.SUCCESS)
  }
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
        stash name: 'snapshots-to-publish', includes: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
        /* Do not defer "archiveArtifacts" to later (like post section),
           as this command must run in the same agent and Docker container
           as build.sh. */
        archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
      }
    }
    // stage('Upload Snapshots') {
    //   /* This must run outside Docker since it directly copies the files. */
    //   agent { label 'web-jenkins' }
    //   when { branch 'master' }
    //   steps {
    //     unstash name: 'snapshots-to-publish'
    //     sh 'jenkins_scripts/upload_snapshots.sh'
    //   }
    // }
  }
  post {
    regression {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
