# (Java) Core

This is the library shared amongst the java parts. 

The primary use is to reduce code-repetition and have a high-tested lib with shared api´s. With this Bib, changes should be faster doable as they don´t need to be doubled. 

The secondary use is to see and play how the maven package will fit into the CI/CD-pipeline. 
Especially things as 2 step builds and pipeline-triggers are the focus of this. 

## Run

It should run with a simple: 

`mvn clean install`

This will run the full process and install it into your local maven repository for other projects to use. This is also the way to get a new unstable-version for changes in two sub-projects.

Its considered best practice to always clean when building bibs.

## Troubleshooting

With Eclipse, I (accidently?) added my maven dependencies to my classpath. 
The classpath has additionally: 

```
<classpathentry kind="con" path="org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER">
		<attributes>
			<attribute name="maven.pomderived" value="true"/>
		</attributes>
</classpathentry>
```

But I am not sure, whether this is actually required or not. 

## Requirements

- Java 12 
- Maven 3
- Internet