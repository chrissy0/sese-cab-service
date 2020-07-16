GNATdoc.SourceFile = {
  "kind": "code",
  "children": [
    {
      "kind": "line",
      "number": 1,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @summary"
        }
      ]
    },
    {
      "kind": "line",
      "number": 2,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Front distance controller package specification."
        }
      ]
    },
    {
      "kind": "line",
      "number": 3,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 4,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @author Julian Hartmer"
        }
      ]
    },
    {
      "kind": "line",
      "number": 5,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @description"
        }
      ]
    },
    {
      "kind": "line",
      "number": 6,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- This controls the front distance by pulling and evaluating the front"
        }
      ]
    },
    {
      "kind": "line",
      "number": 7,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- distance sensor values. Communicates with Motor Controller Task."
        }
      ]
    },
    {
      "kind": "line",
      "number": 8,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 9,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Motor_Controller"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "use"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Motor_Controller"
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
      "number": 10,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "package"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Front_Distance",
          "href": "docs/front_distance___spec.html#L10C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 11,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 12,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Enumartion to reference the front distance sensor by their position."
        }
      ]
    },
    {
      "kind": "line",
      "number": 13,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @value CENTER middle sensor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 14,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @value LEFT left sensor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 15,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @value RIGHT right sensor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 16,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Position_T",
          "href": "docs/front_distance___spec.html#L16C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 17,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "CENTER",
          "href": "docs/front_distance___spec.html#L17C7"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "LEFT",
          "href": "docs/front_distance___spec.html#L17C15"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "RIGHT",
          "href": "docs/front_distance___spec.html#L17C21"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";",
          "href": "docs/front_distance___spec.html#L16C9"
        }
      ]
    },
    {
      "kind": "line",
      "number": 18,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 19,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Type to reference the sensor by their number. Each number is used for"
        }
      ]
    },
    {
      "kind": "line",
      "number": 20,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- one duplicate sensor array."
        }
      ]
    },
    {
      "kind": "line",
      "number": 21,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Number_T",
          "href": "docs/front_distance___spec.html#L21C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Integer"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "range"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "0"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ".."
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "1"
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
      "number": 22,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 23,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Enumartion to reference the sensors by their type."
        }
      ]
    },
    {
      "kind": "line",
      "number": 24,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @value IR infrared sensor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 25,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @value US ultra sonic sensor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 26,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Type_T",
          "href": "docs/front_distance___spec.html#L26C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IR",
          "href": "docs/front_distance___spec.html#L26C27"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "US",
          "href": "docs/front_distance___spec.html#L26C31"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";",
          "href": "docs/front_distance___spec.html#L26C9"
        }
      ]
    },
    {
      "kind": "line",
      "number": 27,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 28,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Acces type to getter function for front distance sensor values."
        }
      ]
    },
    {
      "kind": "line",
      "number": 29,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- The sensor is referenced by type, position and number."
        }
      ]
    },
    {
      "kind": "line",
      "number": 30,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Should only be used by Front_Distance_Task"
        }
      ]
    },
    {
      "kind": "line",
      "number": 31,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param typ sensor type"
        }
      ]
    },
    {
      "kind": "line",
      "number": 32,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param pos sesnor position"
        }
      ]
    },
    {
      "kind": "line",
      "number": 33,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param num sensor number"
        }
      ]
    },
    {
      "kind": "line",
      "number": 34,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @return sensor value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 35,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "get_sensor_value_access",
          "href": "docs/front_distance___spec.html#L35C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "access"
        }
      ]
    },
    {
      "kind": "line",
      "number": 36,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        }
      ]
    },
    {
      "kind": "line",
      "number": 37,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
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
      "number": 38,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "typ",
          "href": "docs/front_distance___spec.html#L38C9"
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
          "text": "Sensor_Type_T",
          "href": "docs/front_distance___spec.html#L26C9"
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
      "number": 39,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "pos",
          "href": "docs/front_distance___spec.html#L39C9"
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
          "text": "Sensor_Position_T",
          "href": "docs/front_distance___spec.html#L16C9"
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
      "number": 40,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "num",
          "href": "docs/front_distance___spec.html#L40C9"
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
          "text": "Sensor_Number_T",
          "href": "docs/front_distance___spec.html#L21C12"
        }
      ]
    },
    {
      "kind": "line",
      "number": 41,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Long_Float"
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
      "number": 42,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 43,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Task to fetch and evaluate front distance sensor values. Communicates"
        }
      ]
    },
    {
      "kind": "line",
      "number": 44,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- with the External Controller Task by calling the entries"
        }
      ]
    },
    {
      "kind": "line",
      "number": 45,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Front_Distance_Next and Front_Distance_Done."
        }
      ]
    },
    {
      "kind": "line",
      "number": 46,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "task"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Front_Distance_Task_T",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L46C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 47,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 48,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Motor Task contrstrucor. Tasks wait after spawning for constructor"
        }
      ]
    },
    {
      "kind": "line",
      "number": 49,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- to initialize the task."
        }
      ]
    },
    {
      "kind": "line",
      "number": 50,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param get_sensor_value_a Front distance sensor getter function"
        }
      ]
    },
    {
      "kind": "line",
      "number": 51,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param us_thresh_front ultrasonic front distance detection threshhold value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 52,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param ir_thresh_front infrared front distance detection threshhold value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 53,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param us_thresh_side ultrasonic side distance detection threshhold value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 54,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param ir_thresh_side infrared side distance detection threshhold value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 55,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param Motor_Controller_Task_A Access to motor controller task"
        }
      ]
    },
    {
      "kind": "line",
      "number": 56,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param timeout_v Rendezvous synchronization timeout"
        }
      ]
    },
    {
      "kind": "line",
      "number": 57,
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
          "href": "docs/front_distance___front_distance_task_t___spec.html#L57C13"
        }
      ]
    },
    {
      "kind": "line",
      "number": 58,
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
      "number": 59,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "get_sensor_value_a",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L59C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
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
          "text": "get_sensor_value_access",
          "href": "docs/front_distance___spec.html#L35C9"
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
      "number": 60,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "us_thresh_front",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L60C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "          "
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
          "text": "Long_Float"
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
      "number": 61,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ir_thresh_front",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "          "
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
          "text": "Long_Float"
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
      "number": 62,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "us_thresh_side",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L62C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "           "
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
          "text": "Long_Float"
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
      "number": 63,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ir_thresh_side",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L63C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "           "
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
          "text": "Long_Float"
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
      "number": 64,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "         "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Motor_Controller_Task_A",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L64C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
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
      "number": 65,
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
          "href": "docs/front_distance___front_distance_task_t___spec.html#L65C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                "
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
      "number": 66,
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
    },
    {
      "kind": "line",
      "number": 67,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "end"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Front_Distance_Task_T",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L46C14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";",
          "href": "docs/front_distance___front_distance_task_t___spec.html#L46C14"
        }
      ]
    },
    {
      "kind": "line",
      "number": 68,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 69,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "private"
        }
      ]
    },
    {
      "kind": "line",
      "number": 70,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 71,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Array to store all sensor values."
        }
      ]
    },
    {
      "kind": "line",
      "number": 72,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "All_Sensor_Values_Array_T",
          "href": "docs/front_distance___spec.html#L72C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "array"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Type_T",
          "href": "docs/front_distance___spec.html#L26C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Position_T",
          "href": "docs/front_distance___spec.html#L16C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Number_T",
          "href": "docs/front_distance___spec.html#L21C12"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "of"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Long_Float"
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
      "number": 73,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 74,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Array to store all sensor threshholds."
        }
      ]
    },
    {
      "kind": "line",
      "number": 75,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Threshhold_Array_T",
          "href": "docs/front_distance___spec.html#L75C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "array"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Type_T",
          "href": "docs/front_distance___spec.html#L26C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Sensor_Position_T",
          "href": "docs/front_distance___spec.html#L16C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "of"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Long_Float"
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
      "number": 76,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 77,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Returns the Front_Distance_Done_T Signal from sensor data."
        }
      ]
    },
    {
      "kind": "line",
      "number": 78,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- It returns FD_FAULT_S when all sensor types are faulty. A sensor type"
        }
      ]
    },
    {
      "kind": "line",
      "number": 79,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- is fault, when every sensor of this type at one position is faulty."
        }
      ]
    },
    {
      "kind": "line",
      "number": 80,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- If a sensor type is not faulty and at least one sensor detects an object,"
        }
      ]
    },
    {
      "kind": "line",
      "number": 81,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- the function returns FRONT_BLOCKED. In every other case, it returns"
        }
      ]
    },
    {
      "kind": "line",
      "number": 82,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- FRONT_CLEAR."
        }
      ]
    },
    {
      "kind": "line",
      "number": 83,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param all_sensor_values array containing all sensor values"
        }
      ]
    },
    {
      "kind": "line",
      "number": 84,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @param threshholds array with threshholds for each sensor type"
        }
      ]
    },
    {
      "kind": "line",
      "number": 85,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- @return Done Signal value"
        }
      ]
    },
    {
      "kind": "line",
      "number": 86,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "calculate_output",
          "href": "docs/front_distance___spec.html#L86C13"
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
          "text": "     "
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
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "all_sensor_values",
          "href": "docs/front_distance___spec.html#L88C7"
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
          "text": "All_Sensor_Values_Array_T",
          "href": "docs/front_distance___spec.html#L72C9"
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
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "threshholds",
          "href": "docs/front_distance___spec.html#L89C7"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
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
          "text": "Threshhold_Array_T",
          "href": "docs/front_distance___spec.html#L75C9"
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
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Front_Distance_Done_t",
          "href": "docs/motor_controller___spec.html#L59C9"
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
          "cssClass": "keyword",
          "text": "end"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Front_Distance"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    }
  ],
  "label": "front_distance.ads"
};