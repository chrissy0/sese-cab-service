/*
 * File:          WebotsController.c
 * Date:
 * Description:
 * Author:
 * Modifications:
 */
 #undef UNICODE
 
 /* TODO: Fix this CaMeL CaSe under_score MESS
   Rename client -> external_controller
   Fix intendation
   use Clang
 */

#define WIN32_LEAN_AND_MEAN
//#pragma comment (lib, "Ws2_32.lib")
// #pragma comment (lib, "Mswsock.lib")
/*
 * You may need to add include files like <webots/distance_sensor.h> or
 * <webots/motor.h>, etc.
 */
#include <stdint.h>
#include <inttypes.h>
#include <webots/robot.h>
#include <webots/distance_sensor.h>
#include <stdio.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <webots/device.h>
#include <webots/motor.h>

/* Constants */
#define TIME_STEP 20
#define DEFAULT_PORT "27015"
#define register_sensor 0
#define WC2EC_CMD_WC2EC_CMD_SEND_SENSOR_DATA 1
#define WC2EC_SET_VALUE 130
#define NUM_SOCKET_BUFFERS 1
#define SOCKET_BUFFER_SIZE 4096
#define SENSOR_DOES_NOT_PROVIDE_DATA 0
#define PEER_CLOSED_CONNECTION 0

#define WC2EC_DEBUG_RECEIVE 0
#define WC2EC_DEBUG_INIT 0

/* Structs */
struct package_format {
  uint8_t command;
  uint8_t sensor_type;
  uint8_t sensor_id;
  char data[];
 }__attribute__((packed));
 

 int setup_connection(SOCKET *listen_socket, SOCKET *client_socket, const char* servname) {
    WORD wVersionRequired;
    WSADATA wsaData;
    struct addrinfo *result = NULL;
    struct addrinfo hints;
    
    wVersionRequired = MAKEWORD(2,2);
    if (WSAStartup(wVersionRequired, &wsaData)) {
        printf("WSAStartup failed\n");
        return 1;
    }
    
    ZeroMemory(&hints, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_PASSIVE;

    if (getaddrinfo(NULL, servname, &hints, &result)) {
        printf("getaddrinfo failed\n");
        goto WSA_CLEANUP;
    }
      
    *listen_socket = socket((result)->ai_family, (result)->ai_socktype, (result)->ai_protocol);
    if (*listen_socket == INVALID_SOCKET) {
        printf("socket failed with error: %d\n", WSAGetLastError());
        goto FREE_ADDRINFO;
    }
    
    if (bind(*listen_socket, (result)->ai_addr, (int)(result)->ai_addrlen) == SOCKET_ERROR) {
        printf("bind failed with error: %d\n", WSAGetLastError());
        goto CLOSE_SOCKET;
    }
 
    printf("Waiting for the external Controller\n");
    printf("Connect to: 127.0.0.1:%s\n", servname);
    /*Needed to print the printf */
    wb_robot_step(1);
    
    if (listen(*listen_socket, SOMAXCONN) == SOCKET_ERROR) {
        goto CLOSE_SOCKET;
    }


    *client_socket = accept(*listen_socket, NULL, NULL);
    if (*client_socket == INVALID_SOCKET) {
        printf("accept failed with error: %d\n", WSAGetLastError());
        goto CLOSE_SOCKET;
    }


    freeaddrinfo(result);
     
    printf("external Controller connected\n");
    /*Needed to print the printf */
    wb_robot_step(1);
    return 0;

CLOSE_SOCKET:
    closesocket(*listen_socket);
FREE_ADDRINFO:
    freeaddrinfo(result);
WSA_CLEANUP:
    WSACleanup();
    return 1;
}

int send_package(struct package_format* pf, uint32_t package_length, SOCKET* client_socket) {
    int send_result;
    uint32_t offset, bytes_to_send = package_length;

    while(bytes_to_send) {
        offset = package_length - bytes_to_send;
        send_result = send(*client_socket, ((char*) pf) + offset, bytes_to_send, 0);
        if (send_result == SOCKET_ERROR) {
                printf("send failed with error: %d\n", WSAGetLastError());
                return -1;
        }
        bytes_to_send -= send_result;
    }
    return 0;
}
int register_sensor_at_external_controller(WbDeviceTag tag, uint8_t index, SOCKET* client_socket) {
    struct package_format *pf;
    uint32_t package_length;
    const char *name;

    name = wb_device_get_name(tag);   
    package_length = sizeof(struct package_format) + strlen(name) + sizeof(uint8_t);
    pf = malloc(package_length);
    if (! pf)
      return -ENOMEM;
      
    pf->command = register_sensor;
    pf->sensor_type = wb_device_get_node_type(tag);
    pf->sensor_id = index;
    *(pf->data) = strlen(name);
    memcpy(pf->data + 1, name, strlen(name));
    send_package(pf, package_length, client_socket);
    
    return 0;
}

int init_sensors(SOCKET* client_socket) {
    /* Current implementation uses an uint8_t for the the device id
     * This can be extended easily, but it is a nasty bug, so just
     * insert a small reminder
     */
    WbDeviceTag tag;
    WbNodeType type;
    const char *name;
    int n_devices = wb_robot_get_number_of_devices();
    
    if (wb_robot_get_number_of_devices() > UINT8_MAX)
        return -1;

    for(int index = 0; index < n_devices; index++) {
        tag = wb_robot_get_device_by_index(index);
        name = wb_device_get_name(tag);
        type = wb_device_get_node_type(tag);
        
        if (WC2EC_DEBUG_INIT)
            printf("Enabled Device #%d name = %s\n", index, name);
            
        register_sensor_at_external_controller(tag, index, client_socket);
        switch(type) {
            case WB_NODE_DISTANCE_SENSOR:
                wb_distance_sensor_enable(tag, TIME_STEP);
                break;
            case WB_NODE_ROTATIONAL_MOTOR:
                 wb_motor_set_position(tag, INFINITY);
                 wb_motor_set_velocity(tag, 0.0);
                 break;
             default:
                 printf("Cant enable unknown sensor of type: %d\n", type);
                 return -1;
         }   
      }
      return 0;
}

int32_t get_data_length_for_sensor_type(WbNodeType type) {
    int32_t data_length = 0;
    
    switch(type) {
        case WB_NODE_DISTANCE_SENSOR:
            data_length = sizeof(double);
            break;
        case WB_NODE_ROTATIONAL_MOTOR:
            data_length = 0;
            break;
        default:
            data_length = -1;
            break;
    }
    
    return data_length;
}

int32_t get_sensor_data_for_sensor_type(WbNodeType type, WbDeviceTag tag,
                                         uint32_t data_length, char data[]) {
    switch(type) {
    case WB_NODE_DISTANCE_SENSOR: 
        if (sizeof(double) != data_length)
            return -1;    

        *((double*) data) = wb_distance_sensor_get_value(tag);
        break;
    default:
            printf("Cant get value for unknown sensor of type: %d\n", type);
            return -1;
    }   
    return 0;
}
int send_sensor_data(SOCKET* client_socket) {
    /* Current implementation uses an uint8_t for the the device id
     * This can be extended easily, but it is a nasty bug, so just
     * insert a small reminder
     */
    uint8_t n_devices;
    WbDeviceTag tag;
    WbNodeType type;
    struct package_format *pf;
    int32_t data_length, package_length;
    
    if (wb_robot_get_number_of_devices() > UINT8_MAX)
        return -1;
    
    n_devices = wb_robot_get_number_of_devices();
        
    for(int index = 0; index < n_devices; index++) {
        tag = wb_robot_get_device_by_index(index);
        type = wb_device_get_node_type(tag);
      
        data_length = get_data_length_for_sensor_type(type);
        
        if (data_length == SENSOR_DOES_NOT_PROVIDE_DATA)
            continue;
        else if (data_length < 0)
            return -1;
            
        package_length = sizeof(struct package_format) + data_length;
        if (! (pf = malloc(package_length))) 
            return -ENOMEM;
            
        pf->command = WC2EC_CMD_WC2EC_CMD_SEND_SENSOR_DATA;
        pf->sensor_type = type;
        pf->sensor_id = index;
        if (get_sensor_data_for_sensor_type(type, tag, data_length,  (pf->data))) {
            free(pf);
            printf("Could not get sensor data!\n");
            printf("For Type :%d\n Tag: %u\n", type, tag);
            printf("Data Length: %d\n", data_length);
            continue;
        }
       
        if (send_package(pf, package_length, client_socket)) {
            printf("send_package failed!\n");
            printf("For Type :%d\n Tag: %u\n", type, tag);
            printf("Data Length: %d\n", data_length);
            free(pf);
            return -1;
        }
      }
      return 0;
}
long unsigned int execute_command(struct package_format* pkg) {
      WbDeviceTag tag;
 
      if (pkg->command == WC2EC_SET_VALUE && pkg->sensor_type == WB_NODE_ROTATIONAL_MOTOR) {
   
          /* TODO errorhandling */
          tag = wb_robot_get_device_by_index(pkg->sensor_id);
          
          if (WC2EC_DEBUG_RECEIVE) {
              printf("Received Motor command\n");
              printf("Setting speed to: %f\n", *((double*) pkg->data));
              printf("For %s\n", wb_device_get_name(tag)); 
          }

          wb_motor_set_velocity(tag, *((double*) pkg->data));  
          return sizeof(double) + sizeof(struct package_format);
      }
      else {
          if (WC2EC_DEBUG_RECEIVE)
              printf("expected: %x, %x\n", WC2EC_SET_VALUE, WB_NODE_ROTATIONAL_MOTOR);
      }
      return 0; 
}
int receive_commands(SOCKET *client_socket, LPWSABUF *socket_buffer) {
    long unsigned int recvBytes, flags = 0, consumed = 0;
    struct package_format *recvPkg;
    FD_SET ReadSet;
    TIMEVAL timeout = { .tv_sec = 0, 
                        .tv_usec = 0
                      };

    FD_ZERO(&ReadSet);
    FD_SET(*client_socket, &ReadSet);
    if ((select(1, &ReadSet, NULL, NULL, &timeout)) == SOCKET_ERROR)
    {
         printf("select() returned with error %d\n", WSAGetLastError());
         return -1;
    }
    if (FD_ISSET(*client_socket, &ReadSet)) {
    /*
     * Enter here functions to send actuator commands, like:
     * wb_motor_set_position(my_actuator, 10.0);
     */
        if (WSARecv(*client_socket, *socket_buffer, NUM_SOCKET_BUFFERS, &recvBytes, &flags, NULL, NULL) == SOCKET_ERROR)
        {
            if (WSAGetLastError() != WSAEWOULDBLOCK)
            {
                  printf("WSARecv() failed with error %d\n", WSAGetLastError());
                  return -1;
            }
        }
        else {

            if (recvBytes == PEER_CLOSED_CONNECTION)
            {
                printf("external controller closed connection\n");
                return -1;
            }
            
            if (WC2EC_DEBUG_RECEIVE) {
              printf("Received: %lu bytes\n", recvBytes);
              for (int i = 0; i < recvBytes; i++) {
                printf("%x\n", (*socket_buffer)->buf[i]);
              }
            }
            
             /* TODO handle partial receive case
              * e.g. a package is split into two receives... ugly
              */
            while (recvBytes != consumed) {
                recvPkg = (struct package_format*) ((*socket_buffer)->buf + consumed);
                if (WC2EC_DEBUG_RECEIVE) {
                  printf("command: %u\n", recvPkg->command);
                  printf("sensorType: %u\n", recvPkg->sensor_type);
                  printf("sensorID: %u\n", recvPkg->sensor_id);
                  printf("consumed: %lu\n", consumed);
                }
                consumed += execute_command(recvPkg);
            }
        }
    } 
    return 0;
}
int init_socket_buffer(LPWSABUF *socket_buffer) {
    *socket_buffer = malloc(sizeof(struct _WSABUF));
    if (! *socket_buffer)
        return -ENOMEM;
     
    (*socket_buffer)->len = SOCKET_BUFFER_SIZE;
    (*socket_buffer)->buf = malloc(SOCKET_BUFFER_SIZE);
    if(! (*socket_buffer)->buf) {
        free(*socket_buffer);
        return -ENOMEM;
    }
    return 0;
}

void wc2ec_cleanup_socket_buffer(LPWSABUF* socket_buffer) {
    if (! *socket_buffer)
        return;
    if ((*socket_buffer)->buf) {
        free((*socket_buffer)->buf);
    }
    free(*socket_buffer);
    *socket_buffer = NULL;
}
void wc2ec_cleanup_sockets(SOCKET* listen_socket, SOCKET* client_socket) {
    closesocket(*client_socket);
    closesocket(*listen_socket);
    WSACleanup();
}
void wc2ec_cleanup(LPWSABUF* socket_buffer, SOCKET* listen_socket, SOCKET* client_socket) {
    wc2ec_cleanup_socket_buffer(socket_buffer);
    wc2ec_cleanup_sockets(listen_socket, client_socket);
}
int wc2ec_init(LPWSABUF* socket_buffer, SOCKET* listen_socket, SOCKET* client_socket) {

    if (init_socket_buffer(socket_buffer)) {
        printf("socket buffer initialization failed\n");
        return -ENOMEM;
    }
        
    if (setup_connection(listen_socket, client_socket, DEFAULT_PORT)) {
        printf("setting up the connection failed\n");
        wc2ec_cleanup_socket_buffer(socket_buffer);
        return -1;
    }
        
    if (init_sensors(client_socket)) {
        printf("sensor initialization failed\n");
        wc2ec_cleanup(socket_buffer, listen_socket, client_socket);
        return -1;
    }
    return 0;
}
/*
 * This is the main program.
 * The arguments of the main function can be specified by the
 * "controllerArgs" field of the Robot node
 */
int main(int argc, char **argv) {

    SOCKET listen_socket = INVALID_SOCKET;
    SOCKET client_socket = INVALID_SOCKET;
   
    LPWSABUF socket_buffer;  

    /* necessary to initialize webots stuff */
    wb_robot_init();
    /* TODO check rcs */
    if (wc2ec_init(&socket_buffer, &listen_socket, &client_socket)) {
        printf("WC2EC initialization failed\n");
        goto WB_CLEANUP;
    }


    /* main loop
   * Perform simulation steps of TIME_STEP milliseconds
   * and leave the loop when the simulation is over
   */
    while (wb_robot_step(TIME_STEP) != -1) {
 
       send_sensor_data(&client_socket);
       if(receive_commands(&client_socket, &socket_buffer)) {
           printf("Receive error\n");
           goto TERMINATE_CONTROLLER;  
       }  
    }


TERMINATE_CONTROLLER:
    /* TODO MAYBE STOP ALL WHEELS */
    wc2ec_cleanup(&socket_buffer, &listen_socket, &client_socket);
WB_CLEANUP:
    wb_robot_cleanup();
    return 0;
}
