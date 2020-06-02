pipeline {
  agent any
  stages {
    stage('External Controller') {
      steps {
        dir(path: 'external_controller') {
          sh './run_test.sh Ring_Buffer_Testing/test_ring_buffer'
        }
      }
    }
    stage('Backend') {
      steps {
        dir(path: 'backend') {
          sh './mvnw -f pom.xml clean test'
        }
      }
    }
  }
}
