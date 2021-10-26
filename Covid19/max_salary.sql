
use learn_SQL;

drop table if exists employee;
create table employee (
	first_name nvarchar(255),
	last_name nvarchar(255),
	department nvarchar(255),
	salary numeric);

insert into employee values 
	('qwe123', 'qaz321', 'A1', 1000),
	('asd456', 'azx654', 'A1', 1500),
	('zxc789', 'zse987', 'B2', 1800),
	('wsx123', 'xsw321', 'B2', 1800),
	('edc135', 'cde531', 'C3', 2000),
	('dfv246', 'vfd642', 'C3', 3000);

 --select * from employee;

--select *
--from
--	(select 
--		*,
--		max(salary) over (partition by department) as max_salary
--	from employee) as max_sal
--where salary = max_salary;

--select *
--from
--	(select
--		*,
--		rank() over (partition by department order by salary desc) rank_salary
--	from employee) e
--where rank_salary = 1;

--select * 
--from employee e1
--where exists
--	(select *
--	from (select department, max(salary) as max_salary from employee group by department) e2
--	where e1.department = e2.department and e1.salary = e2.max_salary);

select 
	*,
	--sum(salary) over (partition by department) as dpt_budget,
	salary/sum(salary) over (partition by department) as ratio_salary
from employee;

