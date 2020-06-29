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

### Pickup Person

##### Request

```http
POST /api/ec/requestPickup?cabId=0&markerId=0&customerId=0
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
    "complete": false
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
POST /api/ec/requestDropoff?cabId=0&sectionId=0&customerId=0
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

###### Section ID unknown

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
    "complete": false
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
