GNATdoc.Documentation = {
  "label": "Lane_Detection.Lane_Detection_Taks_T",
  "qualifier": "",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "Task to fetch and evaluate road marker sensor values. Communicates\n"
        },
        {
          "kind": "span",
          "text": "with the Job Executer Task by road_marker_done and road_marker_next.\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Construct",
          "qualifier": "",
          "line": 86,
          "column": 13,
          "src": "srcs/lane_detection.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 86,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "entry"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Construct",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L86C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 87,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 88,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Motor_Task_A",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L88C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "            "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Motor_Controller_Task_Access_T",
                      "href": "docs/motor_controller___spec.html#L153C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 89,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_line_sensor_value_a",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L89C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_line_sensor_value_access",
                      "href": "docs/lane_detection___spec.html#L40C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 90,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_curb_sensor_value_a",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L90C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_curb_sensor_value_access",
                      "href": "docs/lane_detection___spec.html#L54C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 91,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_wall_sensor_value_a",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L91C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "get_wall_sensor_value_access",
                      "href": "docs/lane_detection___spec.html#L68C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 92,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "timeout_v",
                      "href": "docs/lane_detection___lane_detection_taks_t___spec.html#L92C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "               "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Duration"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 93,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Roadmarker task constructor. Tasks wait after spawning for constructor\n"
                },
                {
                  "kind": "span",
                  "text": "to initialize the task.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Motor_Task_A",
              "line": 88,
              "column": 10,
              "type": {
                "label": "Motor_Controller.Motor_Controller_Task_Access_T",
                "docHref": "docs/motor_controller___spec.html#L153C9"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "access to motor task\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "get_line_sensor_value_a",
              "line": 89,
              "column": 10,
              "type": {
                "label": "Lane_Detection.get_line_sensor_value_access",
                "docHref": "docs/lane_detection___spec.html#L40C9"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "access to line sensor getter function\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "get_curb_sensor_value_a",
              "line": 90,
              "column": 10,
              "type": {
                "label": "Lane_Detection.get_curb_sensor_value_access",
                "docHref": "docs/lane_detection___spec.html#L54C9"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "access to curb sensor getter function\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "get_wall_sensor_value_a",
              "line": 91,
              "column": 10,
              "type": {
                "label": "Lane_Detection.get_wall_sensor_value_access",
                "docHref": "docs/lane_detection___spec.html#L68C9"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "access to wall sensor getter function\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "timeout_v",
              "line": 92,
              "column": 10,
              "type": {
                "label": "Duration"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Rendezvous synchronization timeout\n"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "label": "Entries"
    }
  ]
};