package de.tuberlin.sese.cabservice.persistence.debug;

import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Optional;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class DebugServiceTest {

    @Autowired
    private DebugService debugService;

    @MockBean
    private DebugRepo debugRepo;

    @MockBean
    private CabRepo cabRepo;

    @Test
    public void shouldGetCabSensorStatus() {
        SensorStatusEntity entity1 = SensorStatusEntity.builder()
                .id(1L)
                .cabId(1L)
                .name("inf_rm_br")
                .disabled(false)
                .noise(15)
                .build();
        SensorStatusEntity entity2 = SensorStatusEntity.builder()
                .id(2L)
                .cabId(2L)
                .name("inf_rm_br2")
                .disabled(false)
                .noise(15)
                .build();
        SensorStatusEntity entity3 = SensorStatusEntity.builder()
                .id(3L)
                .cabId(1L)
                .name("inf_rm_br")
                .disabled(true)
                .noise(0)
                .build();

        when(debugRepo.findAll()).thenReturn(asList(entity1, entity2, entity3));

        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some cab name")
                .build()));

        CabSensorStatus cabSensorStatus = debugService.getCabSensorStatus(1L);

        verify(debugRepo).findAll();
        verifyNoMoreInteractions(debugRepo);

        assertThat(cabSensorStatus.hasSensorErrors()).isTrue();
        assertThat(cabSensorStatus.getSensors()).hasSize(2);
        assertThat(cabSensorStatus.getSensors().get(0)).isEqualToComparingFieldByField(entity1);
        assertThat(cabSensorStatus.getSensors().get(1)).isEqualToComparingFieldByField(entity3);
    }

    @Test
    public void shouldGetCabSensorStatusWithoutErrors() {
        SensorStatusEntity entity = SensorStatusEntity.builder()
                .id(2L)
                .cabId(2L)
                .name("inf_rm_br2")
                .disabled(false)
                .noise(15)
                .build();

        when(debugRepo.findAll()).thenReturn(singletonList(entity));

        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some cab name")
                .build()));

        CabSensorStatus cabSensorStatus = debugService.getCabSensorStatus(1L);

        verify(debugRepo).findAll();
        verifyNoMoreInteractions(debugRepo);

        assertThat(cabSensorStatus.hasSensorErrors()).isFalse();
        assertThat(cabSensorStatus.getSensors()).isNull();
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdWhenGettingCabSensorStatus() {
        when(cabRepo.findById(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> debugService.getCabSensorStatus(1L))
                .isExactlyInstanceOf(UnknownCabIdException.class);

        verifyNoMoreInteractions(debugRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabIdWhenGettingCabSensorStatus() {
        assertThatThrownBy(() -> debugService.getCabSensorStatus(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);

        verifyNoMoreInteractions(debugRepo);
    }
}
