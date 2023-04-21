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
    /* Makes failure in any paralel job to stop the build,
       instead of needlessly trying to finish on one node,
       when another node already failed. */
    parallelsAlwaysFailFast()
  }
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_organization/castle-engine-cloud-builds-tools/master', threshold: hudson.model.Result.SUCCESS)
  }
  agent none
  parameters {
    /* Danger! Using this makes macOS and Raspbery Pi snapshot downloads
       fail from https://castle-engine.io/view3dscene.php . */
    booleanParam(name: 'jenkins_fast', defaultValue: false, description: 'Use at emergencies, to make pipeline build faster')
  }
  stages {
    /* Build for each platform in parallel.
       See https://stackoverflow.com/questions/43913698/jenkinsfile-parallel-directive
       https://www.jenkins.io/blog/2017/09/25/declarative-1/
       for parallel syntax. */
    stage('Run parallel builds') {
      parallel {
        stage('Build Linux, Windows, Src') {
          agent {
            docker {
              image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
            }
          }
          steps {
            sh "repository_cleanup . --remove-unversioned"
            sh 'jenkins_scripts/build.sh'
            /* Do not defer "archiveArtifacts" to later (like post section),
               as this command must run in the same agent and Docker container
               as build.sh. */
            archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
          }
        }

        stage('Raspberry Pi') {
          when { not { expression { return params.jenkins_fast } } }
          agent {
            label 'raspberry-pi-cge-builder'
          }
          environment {
            CASTLE_ENGINE_PATH = "${WORKSPACE}/castle_game_engine"
            PATH = "${PATH}:${CASTLE_ENGINE_PATH}/bin"
          }
          stages {
            stage('Cleanup (Raspberry Pi)') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('Setup CGE (Raspberry Pi)') {
              steps {
                copyArtifacts(projectName: 'castle_game_engine_organization/castle-engine/master', filter: 'castle-engine*-linux-arm.zip')
                sh 'unzip castle-engine*-linux-arm.zip'
              }
            }
            stage('Build (Raspberry Pi)') {
              steps {
                sh 'jenkins_scripts/build.sh linux arm'
                archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
              }
            }
          }
        }

        stage('macOS') {
          when { not { expression { return params.jenkins_fast } } }
          agent {
            label 'mac-cge-builder'
          }
          environment {
            CASTLE_ENGINE_PATH = "${WORKSPACE}/castle_game_engine"
            PATH = "${PATH}:${CASTLE_ENGINE_PATH}/bin"
          }
          stages {
            stage('Cleanup (macOS)') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('Setup CGE (macOS)') {
              steps {
                copyArtifacts(projectName: 'castle_game_engine_organization/castle-engine/master', filter: 'castle-engine*-darwin-x86_64.zip')
                sh 'unzip castle-engine*-darwin-x86_64.zip'
              }
            }
            stage('Build (macOS)') {
              steps {
                sh 'jenkins_scripts/build.sh darwin x86_64'
                archiveArtifacts artifacts: 'view3dscene-*.tar.gz,view3dscene-*zip,view3dscene-*.apk'
              }
            }
          }
        }
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
