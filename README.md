# Retail Inventory and Sales Management System

The Retail Inventory and Management System is a desktop application developed entirely in a standard Windows environment using the suite of tools available from Progress Software Corporation. It is intended to simplify and enhance retail operations.

It has an easy-to-use interface and allows an administrator to easily manage inventory and produce sales reports, all while allowing sales staff to quickly and easily enter sales data.

It uses a multi-user database, allowing multiple users to work on the application at the same time with no worry of data collision.

---

## User Flow Diagram

<p align="center">
  <img width="600" alt="User Flow Diagram" src="https://github.com/user-attachments/assets/2041d9c7-6092-44d3-bb99-1b90cb6f0390">
</p>

---
##Project Demo: https://www.youtube.com/watch?v=bgq-fXPPKO0
## System Architecture

<p align="center">
  <img width="400" alt="System Architecture" src="https://github.com/user-attachments/assets/79adc2f4-0ed2-41ae-9093-e12e17251648">
</p>

### The Client Tier (User Interface)
This tier represents the user-facing portion of the application which runs locally on the desktops of end-users. In this system, the client tier is responsible for the presentation and execution of the business logic.

-   **User Interface (UI):** This part consists of all the visible elements with which users will interact: login screens, product maintenance, sales entry, and reports. The UI is constructed using a standard set of AppBuilder widgets such as FRAMES, BROWSE tables, FILL-IN fields, and BUTTONS.

### The Server Tier (Data Layer)
This tier is the back end of the system and is responsible for the storage and management of data within the application.

-   **Business Logic:** The business logic is written in Progress 4GL / OpenEdge ABL (version 12.8) and exists as part of the client application within all the screens. When a user selects an action (logging in, adding a product, recording a sale, etc.), the ABL code runs directly on the user's computer.
-   **Database:** The system utilizes a Progress OpenEdge RDBMS database, which is a multi-user database, allowing many client applications to connect and perform work at the same time without data corruption.

---

## Application Interface

<p align="center">
  <img width="1069" alt="Application Interface" src="https://github.com/user-attachments/assets/33a00b25-3053-417c-b8dc-442e00239308">
</p>

---

## Database Design

<p align="center">
  <img width="1192" alt="Database Design" src="https://github.com/user-attachments/assets/f3ddf418-e130-4fce-8d52-c7a757e8785f">
</p>

### Table: `retailuser`
- **Database:** `retaildb`
- **Description:** The `retailuser` table is designed to store user-specific data, including details about usernames, passwords, attempts, and administrative statuses (Admin, Salesperson). The purpose of this table is to manage user accounts within the application, providing all necessary fields for user identification, authentication, and access control.

### Table: `ProductMaster`
- **Database:** `retaildb`
- **Description:** The `ProductMaster` table stores information about inventory. It includes fields such as `productid` (a unique product identifier), `productname`, `category`, `price`, and `stockquantity`. The `productid` field is the primary key and is always greater than zero. Additionally, the table is indexed by both `productid` and `productname` to allow for quick searches and retrieval.

### Table: `SalesTransaction`
- **Database:** `retaildb`
- **Description:** The `SalesTransaction` table stores records of individual sales transactions. It contains fields including `transactionid` (a unique identifier for each sale), `transactiondate`, `productid` (a reference to the `ProductMaster` table), `quantitysold`, and `totalprice`. The `transactionid` serves as the primary key and is always greater than zero. The table also has an index on `productid` to allow for fast queries by product.
