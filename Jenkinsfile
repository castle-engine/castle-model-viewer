/* -*- mode: groovy -*-
  Configure how to run our job in Jenkins.
  See https://castle-engine.io/cloud_builds_jenkins .
*/

pipeline {
  options {
    /* We do not really have a problem with concurrent builds (jenkins_scripts/build.sh
       could execute in parallel in multiple checkouts),
       but it seems that view3dscene job can be created many many times in Jenkins
       and get stuck.
       Using disableConcurrentBuilds as a workaround. */
    disableConcurrentBuilds()
  }
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_organization/castle-engine-cloud-builds-tools/master', threshold: hudson.model.Result.SUCCESS)
  }
  agent any
  stages {
    stage('Build Linux, Windows, Src') {
      agent {
        docker {
          image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
        }
      }
      steps {
        sh 'jenkins_scripts/build.sh'
        /* Do not defer "archiveArtifacts" to later (like post section),
           as this command must run in the same agent and Docker container
           as build.sh. */
        archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
      }
    }

    stage('Build Raspberry Pi') {
      agent {
        label 'raspberry-pi-cge-builder'
      }
      steps {
        sh 'jenkins_scripts/build.sh linux arm'
        archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
      }
    }

    stage('Build macOS') {
      agent {
        label 'mac-cge-builder'
      }
      steps {
        sh 'jenkins_scripts/build.sh darwin x86_64'
        archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
      }
    }
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
