pipeline {
  agent any
  stages {
    stage('External Controller') {
      steps {
        dir(path: 'external_controller') {
          sh './run_test.sh Front_Distance_Testing/front_distance_testing'
        },
        dir(path: 'external_controller') {
          sh './run_test.sh Road_Marker_Testing/road_marker_testing'
        },
        dir(path: 'external_controller') {
          sh './run_test.sh Motor_Controller_Testing/motor_controller_testing'
        },
        dir(path: 'external_controller') {
          sh './run_test.sh Lane_Detection_Testing/lane_detection_testing'
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
