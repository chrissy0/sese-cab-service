pipeline {
  agent any
  stages {
    stage('External Controller') {
      steps {
        dir(path: 'external_controller') {
          sh './run_test.sh Front_Distance_Testing/test_ring_buffer'
        },
        dir(path: 'external_controller') {
          sh './run_test.sh Front_Distance_Testing/front_distance_testing'
        },
		
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
