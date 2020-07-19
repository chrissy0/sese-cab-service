# Schnittstellendefinitionen

## EC - Backend

### Register Cab

##### Request

```http
POST /api/ec/registerCab
```

```json
{
    "cabName": "Some Cab Name",
    "section": 0
}
```

##### Response

###### Successful registration

```http
200 OK
```

```json
{
    "id": 0
}
```

###### Name already in use

```http
409 CONFLICT
```

###### Section unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Update CabLocation

##### Request

```http
POST /api/ec/cabLocation?cabId=3
```

```json
{
	"section": 2
}
```

##### Response

```http
200 OK
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Section unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Request Route

*Allows cabs to request a route. Route always contains route to depot, according to current route.*

##### Request

```http
GET /api/ec/requestRoute?id=0&version=0
```

##### Response

###### Route has been updated

```http
200 OK
```

```json
{
    "version": 1,
    "route": [
    	{
            "action": "turn",
            "direction": "right",
            "marker": 1
        },
    	{
            "action": "pickup",
            "customerId": 0,
            "marker": 2
        },
    	{
            "action": "pickup",
            "customerId": 1,
            "marker": 2
        },
    	{
            "action": "turn",
            "direction": "right",
            "marker": 4
        },
    	{
            "action": "dropoff",
            "customerId": 0,
            "marker": 5
        },
    	{
            "action": "pickup",
            "customerId": 2,
            "marker": 5
        },
    	{
            "action": "turn",
            "direction": "right",
            "marker": 7
        },
    	{
            "action": "dropoff",
            "customerId": 1,
            "marker": 8
        },
    	{
            "action": "dropoff",
            "customerId": 2,
            "marker": 8
        },
    	{
            "action": "turn",
            "direction": "left",
            "marker": 10
        },
    	{
            "action": "turn",
            "direction": "left",
            "marker": 12
        },
    	{
            "action": "turn",
            "direction": "right",
            "marker": 14
        },
    	{
            "action": "wait",
            "marker": 0
        }
    ]
}
```

###### No route updates (EC route version == current route version)

```http
200 OK
```

```json
{
    "version": 0
}
```

###### Unknown ID

```http
409 CONFLICT
```

###### EC version < current route version

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

###### Other error / exception (should not happen)

```http
500 Internal Server Error
```

### Pickup Person

##### Request

```http
POST /api/ec/requestPickup?cabId=0&customerId=0
```

##### Response

###### Successful Request

```http
200 OK
```

###### No customer available

```http
409 CONFLICT
```

###### No space in cab

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### All Pickups Complete

##### Request

```http
GET /api/ec/pickupsComplete?cabId=0
```

##### Response

###### Pickups completed

```http
200 OK
```

```json
{
    "completed": true
}
```

###### Pickups in progress

```http
200 OK
```

```json
{
    "completed": false
}
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Dropoff Person

##### Request

```http
POST /api/ec/requestDropoff?cabId=0&customerId=0
```

##### Response

###### Successful Request

```http
200 OK
```

###### No customer in cab

```http
409 CONFLICT
```

###### Cab at unknown location

```http
409 CONFLICT
```

###### Customer ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### All Dropoffs Complete

##### Request

```http
GET /api/ec/dropoffsComplete?cabId=0
```

##### Response

###### Dropoffs completed

```http
200 OK
```

```json
{
    "completed": true
}
```

###### Dropoffs in progress

```http
200 OK
```

```json
{
    "completed": false
}
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Set Blocked

##### Request

```http
POST /api/ec/blocked?cabId=0&blocked=true
```

##### Response

###### Success

```http
200 OK
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Set Functional/Dysfunctional

##### Request

```http
POST /api/ec/functional?cabId=0&functional=false
```

##### Response

###### Success

```http
200 OK
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### Sensor status

##### Considered sensors
- `inf_right`
- `inf_right2`
- `inf_cent`
- `inf_cent2`
- `inf_rm_fr`
- `inf_rm_fr2`
- `inf_rm_fr_act`
- `inf_rm_fr_act2`
- `inf_rm_fl`
- `inf_rm_fl2`
- `inf_rm_fl_act`
- `inf_rm_fl_act2`
- `inf_rm_bl`
- `inf_rm_bl2`
- `inf_rm_bl_act`
- `inf_rm_bl_act2`
- `inf_rm_br`
- `inf_rm_br2`
- `inf_rm_br_act`
- `inf_rm_br_act2`
- `dist_c`
- `dist_c2`
- `curb_rf`
- `curb_rf2`
- `curb_lf`
- `curb_lf2`
- `dist_r`
- `dist_r2`
- `dist_l`
- `dist_l2`
- `dist_ir_l`
- `dist_ir_c`
- `dist_ir_r`
- `dist_ir_l2`
- `dist_ir_c2`
- `dist_ir_r2`
- `inf_left`
- `inf_left2`

##### Request

```http
GET /api/ec/sensorStatus?cabId=1
```

##### Response

###### All sensors working

```http
200 OK
```

```json
{
    "sensorErrors": false
}
```

###### Sensor errors set

*Returns sensors which don't work as usual*

```http
200 OK
```

```json
{
    "sensorErrors": true,
    "sensors": [
    	{
            "name": "some-sensor",
            "disabled": false,
            "whoosh": 5
        },
    	{
            "name": "some-other-sensor",
            "disabled": true
        }
    ]
}
```

###### Cab ID unknown

```http
409 CONFLICT
```

###### Missing parameters/info or malformed request

```http
400 Bad Request
```

### 
