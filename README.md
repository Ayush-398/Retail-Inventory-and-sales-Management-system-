# Retail-Inventory-and-sales-Management-system
The Retail Inventory and Management System is a desktop application developed entirely in a standard Windows envirnoment using 
the suite of tools available from Progress Software Corporation.that is intended to simplify and enhance retail operations. 

It has an easy-to-use interface and allows an administrator to easily manage the 
inventory and produce a sales report all while allowing sales staff to quickly and easily enter sales data. 

It uses a multiuser database, allowing multiple users to work on the application at the same time, with no 
worry of data collision. 

#User flow diagram:-
<img width="600" height="700" alt="image" src="https://github.com/user-attachments/assets/2041d9c7-6092-44d3-bb99-1b90cb6f0390" />

#System Architecture
<img width="400" height="350" alt="image" src="https://github.com/user-attachments/assets/79adc2f4-0ed2-41ae-9093-e12e17251648" />

The Client Tier (User Interface)  
This tier represents the user-facing portion of the application which runs locally on the desktop of end users. 
In this system, the client tier is responsible for the presentation and execution of the business logic. 
• User Interface (User Interface): this part consists of all the visible elements with which users 
will interact: login screens, product maintenance, sales entry, and reports. The UI is 
constructed using a standard set of AppBuilder widgets such as FRAMES, BROWSE tables, FILL
IN fields, and BUTTONS.

The Server Tier (Data Layer)  
This tier is the back end of the system and is responsible for the storage, management, of data within the 
application. 
• Business Logic: The business logic is written in Progress 4GL / OpenEdge ABL (version 12.8), 
and exist as part of the client application within all the screens. When a user selects an action 
(logging in, adding a product, recording a sale, etc...), the ABL code runs directly on the user's 
computer.  
• the system also system utilize a Progress OpenEdge RDBMS database. which is a multi-user database, allowing many client 
applications to connect and perform work at the same time without data corruption.
<img width="1069" height="821" alt="image" src="https://github.com/user-attachments/assets/19ee5ff8-beb3-49a1-acb9-61e1d6df4077" />
#Database design
<img width="1192" height="918" alt="image" src="https://github.com/user-attachments/assets/f3ddf418-e130-4fce-8d52-c7a757e8785f" />


